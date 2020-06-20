# Packages
library(sp)
library(tidyverse)
library(knitr)
library(Hmisc)
library(brew)
library(maragra)
library(knitr)
library(RColorBrewer)
# library(lme4)
library(lfe)
library(restorepoint)
library(regtools)


classify_protection <- function(days_since){
  ifelse(days_since < 21 | days_since > (183+21),
         0,
         ifelse(days_since < 51, 1,
                ifelse(days_since < 81,
                       0.9,
                       ifelse(days_since < 112,
                              0.8,
                              ifelse(days_since < 143,
                                     0.6,
                                     ifelse(days_since <174,
                                            0.4,
                                            ifelse(days_since <= (183+21),
                                                   0.2,
                                                   0)))))))
}


if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  # Read in data
  ab <- maragra::ab
  ab_panel <- maragra::ab_panel
  bairros <- maragra::bairros
  bes <- maragra::bes
  census <- maragra::census
  clinic <- maragra::clinic
  clinic_agg <- maragra::clinic_agg
  mc <- maragra::mc
  workers <- maragra::workers
  
  if(!'weather.RData' %in% dir()){
    weather <- maragra::weather %>%
    mutate(precipitation = ifelse(is.na(precipitation),
                                  0,
                                  precipitation))
    
    # Get lags for weather
    starts <- c(15, 30, 60, 90)
    ends <- c(30, 60, 90, 120)
    for(k in 1:length(starts)){
      for(j in 1:length(ends)){
        this_start <- starts[k]
        this_end <- ends[j]
        if(this_start < this_end){
          precipitation_column_name <- paste0('precipitation_lag_', this_start, '_', this_end)
          weather[,precipitation_column_name] <- NA
          temp_column_name <- paste0('temp_lag_', this_start, '_', this_end)
        }
      }
    }
    for(i in 1:nrow(weather)){
      starts <- c(15, 30, 60, 90)
      ends <- c(30, 60, 90, 120)
      for(k in 1:length(starts)){
        for(j in 1:length(ends)){
          this_start <- starts[k]
          this_end <- ends[j]
          if(this_start < this_end){
            this_date <- weather$date[i]
            end_date <- this_date - this_start
            start_date <- this_date - this_end
            if(start_date >= min(weather$date,na.rm = TRUE) &
               end_date <= max(weather$date, na.rm = TRUE)){
              sub_data <- weather %>%
                filter(date >= start_date,
                       date <= end_date)
              precipitation_column_name <- paste0('precipitation_lag_', this_start, '_', this_end)
              weather[i,precipitation_column_name] <- mean(sub_data$precipitation, na.rm = TRUE)
              temp_column_name <- paste0('temp_lag_', this_start, '_', this_end)
              weather[i,temp_column_name] <- mean(sub_data$temp, na.rm = TRUE)
            }
          }
        }
      }
    }
    save(weather, file = 'weather.RData')
  } else {
    load('weather.RData')
  }
  
  # Create a dictionary to classify protection
  protection_dict <- 
    data_frame(days_since = seq(min(irs$days_since, na.rm = TRUE),
                                max(irs$days_since, na.rm = TRUE),
                                by = 1)) %>%
    mutate(protection = classify_protection(days_since))
  model_data <- 
    ab_panel %>%
    dplyr::select(oracle_number, date, absent, unidade) %>%
    left_join(irs %>% 
                dplyr::select(date, unidade, chemical, days_since), 
              by = c('unidade', 'date')) %>%
    # mutate(protection = ifelse(days_since < 21 |
    #                              days_since > (183+21) |
    #                              is.na(days_since),
    #                            0, 1)) %>%
    left_join(protection_dict) %>%
    mutate(protection = ifelse(is.na(protection), 0, protection)) %>%
    dplyr::select(-days_since) %>%
    group_by(oracle_number) %>%
    mutate(ever_sprayed = length(which(protection > 0)) > 0) %>%
  # Bring in some information on workers
    left_join(workers %>%
                dplyr::select(oracle_number,
                              permanent_or_temporary,
                              department,
                              sex,
                              # date_of_birth,
                              perm_id,
                              census_name_match_score) %>%
                # void the permids of anyone with a match score of greater than 0.25
                mutate(perm_id = ifelse(census_name_match_score > 0.25,
                                        NA,
                                        perm_id))) %>%
    # Bring in some info from the census
    left_join(census %>%
                filter(!duplicated(perm_id)) %>%
                dplyr::select(perm_id,
                              maragra_bairro,
                              maragra_fabrica,
                              education,
                              floor_material)) %>%
    # Bring in some data for weather
    left_join(weather) %>%
    ########## !!!!!!!!!!!!!!! REMOVING 2016
    filter(date <= '2015-12-31')
  
  # Since ad and factory are same, keep same
  model_data$field <- ifelse(model_data$department == 'Field', 
                             'Field worker',
                             'Not field worker')

  model_data <- model_data %>% 
    mutate(group = paste0(permanent_or_temporary, ' ',
                          tolower(field)))

  # Add a month column
  model_data$month_number <- add_zero(format(model_data$date, '%m'), 2)
  
  # Get geographic info for externality analysis
  # Get latitude / longitude into model_data
  model_data <- model_data %>%
    left_join(census %>%
                filter(!duplicated(unidade)) %>%
                dplyr::select(unidade,
                              longitude_aura,
                              latitude_aura),
              by = 'unidade') 
  
  model_data$p <- model_data$protection
  
  # Centers of each bairro
  bairro_loc <- apply(coordinates(maragra::bairros_maragra_bairro), 2, mean)
  
  # Define function for weighting indirect protection
  weighter <- function(x){
    # out <- (1 / (x+0.01))
    out <- 1/x
    return(out)
  }

  # Define function for getting distance between houses
  overall_spatial <- model_data %>%
    group_by(lat = latitude_aura,
             lng = longitude_aura) %>%
    tally %>%
    dplyr::distinct(lat, lng, .keep_all = TRUE) %>%
    dplyr::select(-n) %>%
    filter(!is.na(lat),
           !is.na(lng)) %>%
    bind_rows(data.frame(lat = bairro_loc[2],
                         lng = bairro_loc[1])) %>%
    mutate(x = lng,
           y = lat)
  overall_spatial$overall_id <- 1:nrow(overall_spatial)
  coordinates(overall_spatial) <- ~x+y
  proj4string(overall_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  overall_distances <- spDists(x = overall_spatial, longlat = TRUE)
  
  get_protection <- function(lat, lng, protection, guess_missing = TRUE){
    # Build a dataframe
    df <- data_frame(lat, lng, protection)
    # Get id
    df$id <- 1:nrow(df)

    if(guess_missing){
      # Define function for guessing
      guess <- function(x){
        x <- x %>%
          mutate(lng = ifelse(is.na(lng), 
                              bairro_loc[1],
                              lng)) %>%
          
          mutate(lat = ifelse(is.na(lat), 
                              bairro_loc[2],
                              lat)) 
        return(x)
      }
      # if no geography (due to no census matching, just use average geo)
      df <- guess(df)
    }
    # save for later
    original_df <- df
    
    # Get unique lat/lng pairs
    df <- df %>%
      group_by(lat, lng) %>%
      summarise(protection = mean(protection, na.rm = TRUE))
    
    # Join to overall spatial to get distances
    df <- left_join(df, overall_spatial@data, by = c('lat', 'lng'))
    keeps <- df$overall_id
    distances <- overall_distances[keeps, keeps]
    
    # # Get a spatial version
    # (no longer necessary with above approach)
    # df_spatial <- df %>% filter(!is.na(lng),
    #                             !is.na(lat))
    # 
    # coordinates(df_spatial) <- ~lng+lat
    # proj4string(df_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    # 
    # # Get distances
    # distances <- spDists(x = df_spatial, longlat = TRUE)
    
    # Weight distances and return weighted protection
    out <- apply(distances, 1, function(x){
      val <- df$protection * weighter(x)
      # Infinite means distance was 0/self; remove
      val[is.infinite(val)] <- NA
      sum(val, na.rm = TRUE)})
    right <- data.frame(lat = df$lat[!is.na(df$lat)],
                        lng = df$lng[!is.na(df$lng)],
                        herd = out)
    # Join to original df
    original_df <- original_df %>%
      left_join(right,
                by = c('lat', 'lng'))
    
    # Return just the protection column
    return(original_df$herd)
  }
  
  # For each day, get real, and hypothetical protection scores
  dates <- sort(unique(model_data$date))

  out_list <- list()
  for(i in 1:length(dates)){
    this_date <- dates[i]
    message(this_date)
    
    this_data <- model_data %>%
      filter(date == this_date)
    # Get protection score
    protection_scores <- get_protection(lat = this_data$latitude_aura,
                                        lng = this_data$longitude_aura,
                                        protection = this_data$protection,
                                        guess_missing = TRUE)
    this_data$herd_protection <- protection_scores
    out_list[[i]] <- this_data 
  }
  model_data <- bind_rows(out_list)
  
  # Create a single "protection" score which combines
  # herd protection and individual protection
  model_data$combined_protection  <-
    # Assuming 50 meters
    (model_data$protection * weighter(0.05)) +
    model_data$herd_protection

  
  # Plots of maps
  # Libraries
  library(tidyverse)
  library(raster)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  library(broom)
  library(ggthemes)
  
  # Get each countries shapefile
  countries <- c('Mozambique')
  iso3s <- c('MOZ')
  for(i in 1:length(countries)){
    message('Fetching data for ', countries[i])
    x <- getData(name = 'GADM', level = 2, country = iso3s[i])
    assign(tolower(countries[i]),
           x,
           envir = .GlobalEnv)
  }
  
  # Define which districts are "special" -------------
  specials <- 'Manhiça'
  
  # Mozambique
  mozambique@data$special <- FALSE
  mozambique@data$special[mozambique@data$NAME_2 %in% specials] <- TRUE
  
  # Combine all data into a "long" / "tidy" format
  make_long <- function(x, region = "NAME_2"){
    shp_df <- broom::tidy(x, region = region)
    return(shp_df)
  }
  mozambique_long <- make_long(mozambique) %>% mutate(country = 'Mozambique')
  combined <- mozambique_long
  combined$special <- combined$id %in% specials
  # combined$special[!combined$special] <- NA
  # Get a map of africa to use as a background
  # from the cism package!
  africa <- cism::africa
  africa_long <- make_long(africa, region = 'COUNTRY')
  africa_long$special <- africa_long$id %in% countries
  
  g1 <-
    ggplot() +
    geom_polygon(data = africa_long,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill = grey(0.6),
                 alpha = 1,
                 color = 'white',
                 lwd = 0.3) +
    geom_polygon(data = combined,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill = grey(0.3)) +
    geom_polygon(data = combined %>% filter(!is.na(special) & special),
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = special)) +
    scale_fill_manual(name = '',
                      values = c('darkorange'),
                      na.value = NA) +
    coord_cartesian() +
    ggthemes::theme_map() +
    theme(legend.position = 'none') +
    # redraw country lines
    geom_polygon(data = africa_long,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill = NA,
                 alpha = 1,
                 color = 'white',
                 lwd = 0.3) + coord_map() +
    labs(title = 'i.')
  country_map <- function(the_country = 'Gabon'){
    ggplot(data = combined %>% filter(country == the_country),
           aes(x = long,
               y = lat,
               group = group,
               fill = special)) +
      geom_polygon(#alpha = 0.8,
        lwd = 0.3,
        color = grey(0.6)) +
      theme_map() +
      coord_map() + 
      scale_fill_manual(name = '', values = c('darkgrey', 'darkred')) +
      theme(legend.position = 'none')
  }
  g2 <- country_map('Mozambique') +
    labs(title = 'ii.')
  
  mar <- data.frame(lat = -25.4498802, long = 32.777661)
  man3_fortified <- cism::man3_fortified
  g3 <- ggplot(data = man3_fortified,
               aes(x = long,
                   y = lat)) +
    geom_polygon(aes(group = group),
                 # alpha = 0.8,
                 lwd = 0.3,
                 color = grey(0.6)) +
    geom_point(data = mar,
               aes(x = long,
                   y = lat),
               color = 'red') +
    geom_point(data = mar,
               aes(x = long,
                   y = lat),
               color = 'red',
               size = 4, 
               pch = 1) +
    geom_point(data = mar,
               aes(x = long,
                   y = lat),
               color = 'red',
               size = 6, 
               pch = 1) +
    geom_point(data = mar,
               aes(x = long,
                   y = lat),
               color = 'red',
               size = 9, 
               pch = 1) +
    theme_map() +
    coord_map() + 
    theme(legend.position = 'none') +
    labs(title = 'iii')
  
  # library(ggmap)
  # # if('.hdf.RData' %in% dir()){
  # load('.hdf.RData')
  # # } else {
  # # hdf <- ggmap::get_map(location = c(lon = mar$long, lat = mar$lat), maptype = 'satellite', zoom = 14)
  # #   save(hdf, file = '.hdf.RData')
  # # }
  
  # g4 <- ggmap::ggmap(hdf) +
  #   theme_map() +
  #   labs(title = 'iv')
  
  # map_list <- list(g1,g2,g3,g4)
  
  save.image(file = 'prepared_data.RData')
}

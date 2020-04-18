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

# classify_protection <- function(days_since){
#   x <- 21:204
#   y <- dplyr::percent_rank(x)
#   y <- rev(y)
#   out <- rep(NA, length(days_since))
#   for(i in 1:length(days_since)){
#     out[i] <-ifelse(days_since[i] %in% x, 
#                     y[x == days_since[i]],
#                     0)
#   }
#   out
# }

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
    left_join(weather)
  
    ########## !!!!!!!!!!!!!!! REMOVING 2016 for temp workers
  model_data <- model_data %>%
    filter(permanent_or_temporary == 'Permanent' |
             (permanent_or_temporary == 'Temporary' & date <= '2015-12-31'))

  
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
      mean(val, na.rm = TRUE)}) # used to be sum
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
  
  save.image('checkpoint.RData')
  # Create a single "protection" score which combines
  # herd protection and individual protection


  
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

### !!!!!!!!!!
# Note: this repeats later
# # Final data - only the ever sprayed
# final_data <- model_data
final_data <- model_data %>%
  filter(ever_sprayed)

# Remove christmas
final_data <- final_data %>%
  mutate(day_number = as.numeric(format(date, '%d')))
remove_christmas <- TRUE
if(remove_christmas){
  remove_these <- which(
    (final_data$month_number == '12' &
       final_data$day_number >= 15) |
      final_data$month_number == '01' &
      final_data$day_number <= 15
    
  )
}



# Prepare simulations
if('simulations.RData' %in% dir()){
  load('simulations.RData')
} else {
  
  # See when they should spray
  # time_optimize_df <- 
  #   bes %>%
  #   group_by(date = paste0(format(date, '%m'))) %>%
  #   summarise(p = mean(p))
  time_optimize_df <-
    data.frame(date = seq(as.Date('2020-01-01'),
                          as.Date('2020-12-31'), 
                          by = 1)) %>%
    mutate(month_day = format(date, '%m-%d'))
  time_optimize_df$day_number <- (1:nrow(time_optimize_df))+20
  time_optimize_df$the_protection <- classify_protection(time_optimize_df$day_number)
  time_optimize_df <- time_optimize_df %>% dplyr::select(month_day, the_protection)
  
  
  strategies <- c('Zero', 'Max', 'Time-optimized')
  strategy_list <- list()
  for(s in 1:length(strategies)){
    dates <- sort(unique(final_data$date))
    out_list <- list()
    this_strategy <- strategies[s]
    
    for(i in 1:length(dates)){
      this_date <- dates[i]
      message(this_strategy, '...', this_date)
      
      this_data <- final_data %>%
        filter(date == this_date)
      
      # Define scores
      if(this_strategy == 'Zero'){
        this_data$protection <- 0
        the_protection <- rep(0, length(this_data$protection))
      }
      if(this_strategy == 'Max'){
        this_data$protection <- 1
        the_protection <- rep(1, length(this_data$protection))
      }
      if(this_strategy == 'Time-optimized'){
        this_data$month_day <- format(this_data$date, '%m-%d')
        x <- this_data %>%
          left_join(time_optimize_df)
        the_protection <- x$the_protection
        this_data$protection <- the_protection
      }
      # Get protection score
      protection_scores <- get_protection(lat = this_data$latitude_aura,
                                          lng = this_data$longitude_aura,
                                          protection = the_protection,
                                          guess_missing = TRUE)
      this_data$herd_protection <- protection_scores
      out_list[[i]] <- this_data 
    }
    out <- bind_rows(out_list)
    out$strategy <- this_strategy
    strategy_list[[s]] <- out
  }
  simulations <- bind_rows(strategy_list)
  simulations <- simulations %>%
    bind_rows(final_data %>% mutate(strategy = 'Real'))
  save(simulations, file = 'simulations.RData')
}

combine_protection <- function(df){
  df %>%
    mutate(combined_protection = herd_protection  + (protection * weighter(0.5)))
}

model_data <- model_data %>% combine_protection()
simulations <- simulations %>% combine_protection()

# Some variable things
model_data$rain_var <- model_data$precipitation_lag_15_90
model_data$protection_var <- model_data$combined_protection

simulations$rain_var <- simulations$precipitation_lag_15_90
simulations$protection_var <- simulations$combined_protection

#
# final_data <- model_data
# # Final data - only the ever sprayed
final_data <- model_data %>%
  filter(ever_sprayed) %>%
#   # Temporary fix pending newer data
   filter(date <= '2016-01-01')


final_model <- felm(log(absent+1) ~ protection_var + (rain_var) | oracle_number | 0 | 0,
                    data = final_data)

summary(final_model)
# final_model_sep <- felm(log(absent+1) ~ individual_protection + herd_protection + (rain_var) | oracle_number | 0 | 0,
#                     data = final_data)
# summary(final_model_sep)
final_model_lm <- lm(log(absent+1) ~ protection_var + (rain_var),
                     data = final_data)

clean_up_model <- function(x, multiplier = 100){
  # extract coefficients
  coefs <- data.frame(coef(summary(x)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  ps <- coefs$p.z
  x = broom::tidy(x)
  for(j in 2:(ncol(x) -1)){
    x[,j] <- x[,j] * multiplier
  }
  x$term <- gsub('seasonhigh', 'Malaria season', x$term)    
  x$term <- gsub('seasonlow', 'Low malaria season', x$term)
  # x$term <- gsub('months_since', 'Months since IRS: ', x$term)
  x$term <- gsub('months_since', 'IRS status=', x$term)
  x$term <- gsub('department', 'Department: ', x$term)
  x$term <- gsub('precipitation', 'Precipitation (mm) ', x$term)
  x$term <- gsub('maragra_fabricaTRUE', 'Living at factory', x$term)
  x$term <- gsub('sexM', 'Male', x$term)
  x$term <- gsub('permanent_or_temporaryTemporary', 'Temp contract', x$term)
  x$term <- gsub('rainy_dayTRUE', 'Rainy day', x$term)
  x$term <- gsub('rainyTRUE', 'Rainy day', x$term)
  x$term <- gsub('on_siteTRUE', 'On site', x$term)
  x$term <- gsub('on_siteFALSE', 'Off site', x$term)
  x$term <- gsub('fieldNot field worker', 'Not field worker', x$term)
  x$term <- gsub('herd', 'Herd protection', x$term)
  x$term <- gsub('before_after', 'IRS time=', x$term)
  x$term <- gsub('time_since', 'Months after=', x$term)
  x$term <- gsub('incidence', 'Incidence', x$term)
  x$term <- gsub('group', '', x$term)
  x$term <- gsub('_lag2_3', '8-15 week lag', x$term)
  names(x) <- Hmisc::capitalize(names(x))
  names(x) <- gsub('.', ' ', names(x), fixed = TRUE)
  x$`P value` <- NA
  x$`P value`[1:length(ps)] <- ps
  x <- x %>% 
    mutate(Estimate = paste0(round(Estimate, 3), ' (P',
                             ifelse(`P value` <= 0.001,
                                    '<0.001',
                                    paste0('=', round(`P value`, 3))
                             ), ')')) %>%
    dplyr::select(Term, Estimate)
  x <- x %>% filter(!grepl('sd_', Term))
  x <- x %>% filter(!grepl('10+', Term, fixed = TRUE))
  return(x)
}

make_models_table <- function(model_list, the_caption = "Models with worker fixed effects", type = 'html', multiplier = 100){
  
  out_list <- list()
  for(i in 1:length(model_list)){
    message(i)
    this_model <- names(model_list)[i]
    the_model <- model_list[[which(names(model_list) == this_model)]]
    the_model <- clean_up_model(the_model, multiplier = multiplier)
    names(the_model)[2] <- this_model
    out_list[[i]] <- the_model
  }
  
  df <- out_list[[1]]
  namer <- names(df)[2]
  names(df)[2] <- 'Estimate'
  
  for(i in 2:length(out_list)){
    temp <- out_list[[i]]
    namer[i] <- names(temp)[2]
    names(temp)[2] <- 'Estimate'
    df <- bind_rows(df, temp)
  }
  
  library(kableExtra)
  breaker <- nrow(out_list[[1]])
  lengther <- length(out_list)
  breaks <- c(0, breaker * 1:(lengther))
  k <- kable(df, format = type, caption = the_caption, booktabs = T, longtable = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
  for (i in 1:(length(breaks)-1)){
    message(i)
    k <- k %>%
      group_rows(namer[i], 
                 breaks[i] + 1,
                 breaks[i] + breaker,
                 latex_gap_space = "1.5em") 
  }
  print(k)
}


make_final_table <- function(model, the_caption = "Models with worker fixed effects", type = 'html', multiplier = 100){
  cleaned_up <- clean_up_model(model)
  
  library(kableExtra)
  k <- kable(cleaned_up, format = type, caption = the_caption, booktabs = T, longtable = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))#, 
                                    # "repeat_header"))
  print(k)
}



# Define function for making season
make_season <- function(date){
  out <- ifelse(as.numeric(format(date, '%m')) %in% c(11:12, 1:3),
                'Malaria season', 'Off season')
  out <- factor(out, levels = c('Off season', 'Malaria season'))
  return(out)
}


# Specific phrases
# The total number of unique agregados sprayed during this period was 3,998. 
#Among the 4835 workers for whom we have both reliable absenteeism and residential data, 
length(unique(model_data$oracle_number))
#  634 had their homes fumigated at least once (the majority of workers live off of the facility).
model_data %>% group_by(oracle_number) %>% summarise(ever_sprayed = dplyr::first(ever_sprayed)) %>%
  group_by(ever_sprayed) %>% tally


# Visualizations



# Build up simulations

# Build up variable protection score (simulations)
# 3 strategies
# 1. 0 IRS
# 2. Same quantity, same placement, different timing
# 3. Same quantity, different placement, different timing
# 4. Full IRS coverage
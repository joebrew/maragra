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
  ifelse(days_since < 21 | days_since > (365+21),0,
         (((365+21) - days_since)/(365))
  )
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
    # out <- 1/x
    out <- ifelse(x > 1, 0, 1-x)
    return(out)
  }
  
  # Radial weighter
  radial_weighter <- function(x){
    out <- ifelse(x > 0.4, 0, 1)
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
  
  get_protection <- function(lat, lng, protection, guess_missing = TRUE, radial = FALSE){
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
      # Keep only those within a kilometer
      val <- val[x <= 1]
      mean(val, na.rm = TRUE)
    })
    
    radial_out <- apply(distances, 1, function(x){
      val <- df$protection * radial_weighter(x)
      # Infinite means distance was 0/self; remove
      val[is.infinite(val)] <- NA
      mean(val, na.rm = TRUE)
    })
    
    right <- data.frame(lat = df$lat[!is.na(df$lat)],
                        lng = df$lng[!is.na(df$lng)],
                        herd = out,
                        herd_radial = radial_out)
    # Join to original df
    original_df <- original_df %>%
      left_join(right,
                by = c('lat', 'lng'))
    
    # Return just the protection column
    if(radial){
      return(original_df$herd_radial)
    } else {
      return(original_df$herd)
    }
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
    radial_protection_scores <- get_protection(lat = this_data$latitude_aura,
                                        lng = this_data$longitude_aura,
                                        protection = this_data$protection,
                                        guess_missing = TRUE, radial = TRUE)
    this_data$herd_protection <- protection_scores
    this_data$radial_herd_protection <- radial_protection_scores
    out_list[[i]] <- this_data 
  }
  model_data <- bind_rows(out_list)
  
  
  
  
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
  
  save.image(file = 'prepared_data.RData')
}


model_data$year <- as.numeric(format(model_data$date, '%Y'))

# Add 5th worker type
model_data <- model_data %>%
  # Get grade
  left_join(ab_panel %>% dplyr::distinct(grade, oracle_number, date)) %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  left_join(wages %>% dplyr::distinct(grade, department, year, daily_usd)) 
model_data$group5 <- ifelse(is.na(model_data$daily_usd) & model_data$group != 'Temporary not field worker', 'No wage data', model_data$group)

# As of now, model_data and final_data are the same
# We want to restrict model_data downward a bit (to not include issues that are bad for modeling)
# while keeping final data (since it's useful for simulations)

final_data <- model_data


# Remove christmas
model_data <- model_data %>%
  mutate(day_number = as.numeric(format(date, '%d')))
remove_christmas <- TRUE
if(remove_christmas){
  remove_these <- which(
    (model_data$month_number == '12' &
       model_data$day_number >= 15) |
      model_data$month_number == '01' &
      model_data$day_number <= 15
  )
}

# Remove 2013 temp workers from model data (since some problems)
model_data <- model_data %>% 
  filter(!(permanent_or_temporary == 'Temporary' & year == 2013))
final_data <- final_data %>% 
  filter(!(permanent_or_temporary == 'Temporary' & year == 2013))

# # only the ever sprayed
model_data <- model_data %>%
  filter(ever_sprayed)
final_data <- final_data %>% filter(ever_sprayed)



# Prepare simulations
if('simulations.RData' %in% dir()){
  load('simulations.RData')
} else {
  
  time_optimize_df <-
    data.frame(date = seq(as.Date('2020-01-01'),
                          as.Date('2020-12-31'), 
                          by = 1)) %>%
    mutate(month_day = format(date, '%m-%d'))
  time_optimize_df$day_number <- (1:nrow(time_optimize_df))+20
  time_optimize_df$the_protection <- classify_protection(time_optimize_df$day_number)
  time_optimize_df <- time_optimize_df %>% dplyr::select(month_day, the_protection)
  
  
  strategies <- c('Zero',  'Max', 'Time-optimized')
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


# Add precipitation
model_data$rain_var <- model_data$precipitation_lag_15_90
final_data$rain_var <- final_data$precipitation_lag_15_90
simulations$rain_var <- simulations$precipitation_lag_15_90


# Make the group var
model_data$group4 <- model_data$group
final_data$group4 <- final_data$group
simulations$group4 <- simulations$group
three_transform <- function(x){
  ifelse(grepl('temporary', tolower(x)), 'Temporary worker', x)
}
model_data$group3 <- three_transform(model_data$group)
final_data$group3 <- three_transform(final_data$group)
simulations$group3 <- three_transform(simulations$group)

model_data$group <- model_data$group3
final_data$group <- final_data$group3
simulations$group <- simulations$group3



# final_model <- felm(log(absent+1) ~ protection_var + (rain_var) | oracle_number | 0 | 0,
#                     data = final_data)
final_model <- felm(log(absent+1) ~ protection +
                      herd_protection + 
                      rain_var | oracle_number | 0 | 0,
                    data = model_data)
summary(model_data)
final_model_lm <- lm(log(absent+1) ~ protection +
                       herd_protection + (rain_var),
                     data = model_data)

final_model_radial <- felm(log(absent+1) ~ protection +
                             radial_herd_protection + 
                      rain_var | oracle_number | 0 | 0,
                    data = model_data)
summary(final_model_radial)
final_model_radial_lm <- lm(log(absent+1) ~ protection +
                       radial_herd_protection + (rain_var),
                     data = model_data)

# Model for each worker group
worker_groups <- sort(unique(model_data$group))
model_data_list <- model_list <- model_list_lm  <- 
  radial_model_data_list <- radial_model_list <- radial_model_list_lm <- list()
for (i in 1:length(worker_groups)){
  sub_data <- model_data %>% filter(group == worker_groups[i])
  this_model <- felm(log(absent+1) ~ protection +
                       herd_protection + 
                       rain_var | oracle_number | 0 | 0,
                     data = sub_data)
  this_model_radial <- felm(log(absent+1) ~ protection +
                              radial_herd_protection + 
                       rain_var | oracle_number | 0 | 0,
                     data = sub_data)
  this_model_lm <- lm(log(absent+1) ~ protection +
                        herd_protection + (rain_var),
                      data = sub_data)
  this_model_lm_radial <- lm(log(absent+1) ~ protection +
                               radial_herd_protection + (rain_var),
                      data = sub_data)
  formatted <- broom::tidy(this_model)
  formatted$group5 <- worker_groups[i]
  
  formatted_radial <- broom::tidy(this_model_radial)
  formatted_radial$group5 <- worker_groups[i]
  
  model_list[[i]] <- this_model
  model_list_lm[[i]] <- this_model_lm
  model_data_list[[i]] <- formatted
  
  radial_model_list[[i]] <- this_model_radial
  radial_model_list_lm[[i]] <- this_model_lm_radial
  radial_model_data_list[[i]] <- formatted_radial
}
names(model_list) <- names(model_data_list) <- names(model_list_lm) <-
  names(radial_model_list) <- names(radial_model_data_list) <- names(radial_model_list_lm) <- 
  worker_groups


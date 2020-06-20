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
# 
# # # Elisa: active workers by worker type over time
# pd <- maragra::ab_panel %>%
#   left_join(
#   maragra::workers %>% dplyr::select(oracle_number, permanent_or_temporary)) %>%
#   mutate(type = paste0(permanent_or_temporary, ' ', department)) %>%
#   mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
#   group_by(type, year_month) %>%
#   tally
# 
# ggplot(data = pd,
#        aes(x = year_month,
#            y = n,
#            fill = type,
#            group = type)) +
#   geom_bar(stat = 'identity',
#            position = position_dodge())
# write_csv(pd, '~/Desktop/pd.csv')
# # 
# pd <- maragra::ab_panel %>%
#   left_join(
#     maragra::workers %>% dplyr::select(oracle_number, permanent_or_temporary)) %>%
#   mutate(type = paste0(permanent_or_temporary)) %>%
#   mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
#   group_by(type, year_month) %>%
#   tally
# 
# ggplot(data = pd,
#        aes(x = year_month,
#            y = n,
#            fill = type,
#            color = type)) +
#   geom_line() +
#   geom_point() +
#   xlim(as.Date('2013-01-01'),
#        as.Date('2016-06-01'))
# 
# pd <- maragra::ab_panel %>%
#   left_join(
#     maragra::workers %>% dplyr::select(oracle_number, permanent_or_temporary, sex)) %>%
#   mutate(type = paste0(permanent_or_temporary, ' ', sex)) %>%
#   mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
#   group_by(type, year_month) %>%
#   tally
# 
# pd$type <- factor(pd$type, levels = rev(unique(pd$type)))
# ggplot(data = pd,
#        aes(x = year_month,
#            y = n,
#            fill = type,
#            color = type)) +
#   geom_area(position = 'stack') +
#   xlim(as.Date('2013-01-01'),
#        as.Date('2016-01-31')) +
#   geom_vline(xintercept = as.Date(paste0(2013:2016, '-01-01')))
# 
# 
# 
# pd <- maragra::ab_panel %>%
#   left_join(
#     maragra::workers %>% dplyr::select(oracle_number, permanent_or_temporary, sex)) %>%
#   mutate(type = paste0(permanent_or_temporary, ' ', sex)) %>%
#   mutate(year_month = as.Date(paste0(format(date, '%Y-%m'), '-01'))) %>%
#   group_by(type, year_month) %>%
#   summarise(n = length(unique(oracle_number)))
# 
# pd$type <- factor(pd$type, levels = rev(unique(pd$type)))
# ggplot(data = pd,
#        aes(x = year_month,
#            y = n,
#            fill = type,
#            color = type)) +
#   geom_area(position = 'stack') +
#   xlim(as.Date('2013-01-01'),
#        as.Date('2017-01-31')) +
#   geom_vline(xintercept = as.Date(paste0(2013:2016, '-01-01')))


# Menno: tabulation of how many times an onsite household was sprayed during the study period
# n_houses <- census %>% filter(maragra_bairro | maragra_fabrica) %>% summarise(x = length(unique(unidade))) %>% .$x
# n_houses <- length(unique(mc$unidade))
# pd <- mc %>%
#   mutate(year = as.numeric(format(date, '%Y'))) %>%
#   filter(year %in% 2013:2016) %>%
#   group_by(year) %>%
#   summarise(unidades = length(unique(unidade)),
#             fumigations = n()) %>%
#   mutate(p_unidades = unidades / n_houses * 100) %>%
#   mutate(fumigations_per_unidade = fumigations / unidades)
# x <- t(pd[,-1])
# x <- data.frame(x)
# colnames(x) <- unlist(pd[,1])
# x
# pd2 <- mc %>%
#   mutate(year = as.numeric(format(date, '%Y'))) %>%
#   filter(year %in% 2013:2016) %>%
#   group_by(year, unidade) %>%
#   tally %>%
#   group_by(year) %>%
#   summarise(`average sprays among spar` = mean(n))



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
      # Keep only those within a kilometer
      val <- val[x <= 1]
      mean(val, na.rm = TRUE)
      
      })
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

# Some variable things
model_data$rain_var <- model_data$precipitation_lag_15_90

# # Final data - only the ever sprayed
final_data <- model_data %>%
  filter(ever_sprayed)
#   # Temporary fix pending newer data
#   filter(date <= '2016-01-01')


# final_model <- felm(log(absent+1) ~ protection_var + (rain_var) | oracle_number | 0 | 0,
#                     data = final_data)
final_model <- felm(log(absent+1) ~ protection +
                      herd_protection + 
                      rain_var | oracle_number | 0 | 0,
                    data = final_data)
summary(final_model)
final_model_lm <- lm(log(absent+1) ~ protection +
                       herd_protection + (rain_var),
                     data = final_data)

library(broom)
broom::tidy(final_model)

# Prediction based on individual stuff
fake <- expand.grid(protection = seq(0, 1, 0.5),
                    herd_protection = seq(0, 1, 0.1),
                    rain_var = seq(0, 6, 1))
fe <- getfe(final_model)
predictions <- exp(predict(final_model_lm, newdata = fake, interval = 'confidence'))-1
predictions <- data.frame(predictions)
fixed_effects <- 0.067 # taken from below
fake$fit <- predictions$fit + fixed_effects
fake$lwr <- predictions$lwr + fixed_effects
fake$upr <- predictions$upr + fixed_effects

n_cols <- length(unique(fake$protection))
cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, 'Spectral'))(n_cols)
cols[2] <- 'black'
library(maragra)
ggplot(data = fake %>% filter(rain_var == 3) %>% mutate(protection = factor(protection)),
       aes(x = herd_protection,
           y = fit,
           color = protection,
           group = protection)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr,
                  fill = protection),
              color = NA,
              alpha = 0.15) +
  geom_point() +
  geom_line(alpha = 0.6) +
  scale_color_manual(name = 'Individual protection',
                     values = cols) +
  scale_fill_manual(name = 'Individual protection',
                     values = cols) +
  labs(x = 'Community protection score',
       title = 'Association between community protection, individual protection, and absenteeism',
       y = 'Estimated monthly absenteeism rate\n(Average fixed effect, 3 cm of 15-60 day prior precipitation)') + theme_maragra() 
  


# SIMULATIONS
sim_data <- simulations
sim_data$idx <- sim_data$oracle_number
fe <- getfe(final_model)
predictions <- exp(predict(final_model_lm, sim_data, interval = 'confidence'))-1
predictions <- data.frame(predictions)
# Add the fixed effects
fixed_effects <- left_join(sim_data,
                           data.frame(fe) %>% dplyr::select(idx, effect) %>%
                             mutate(idx = as.character(idx))) %>%
  .$effect
fixed_effects <- as.numeric(fixed_effects)

predictions$fit <- predictions$fit + fixed_effects
predictions$lwr <- predictions$lwr + fixed_effects
predictions$upr <- predictions$upr + fixed_effects

# combine other info
predictions <- predictions %>% bind_cols(sim_data %>% dplyr::select(date, absent, strategy, permanent_or_temporary)) %>%
  mutate(absent = as.numeric(absent)) %>%
  filter(!is.na(fit))

simple <- predictions %>%
  group_by(strategy) %>%
  summarise(n = n(),
            actual_absences = sum(absent),
            predicted_absences = sum(fit, na.rm = TRUE),
            actual = mean(absent, na.rm = TRUE),
            predicted = mean(fit, na.rm = TRUE)) %>%
  dplyr::rename(value = predicted) %>%
  dplyr::select(strategy, value, predicted_absences)

zero <- predictions %>%
  filter(strategy == 'Zero') %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year) %>%
  summarise(n = n(),
            actual_absences = sum(absent),
            predicted_absences = sum(fit, na.rm = TRUE),
            actual = mean(absent, na.rm = TRUE),
            predicted = mean(fit, na.rm = TRUE)) %>%
  mutate(avoided = predicted_absences - actual_absences)
# dplyr::rename(value = predicted) %>%
# dplyr::select(permanent_or_temporary, value, predicted_absences)

kable(simple)

# Eligible working days
sum(zero$n)
# Absences
sum(zero$actual_absences)
# Actual absence rate
sum(zero$actual_absences) / sum(zero$n)
# Would have observed
sum(zero$predicted_absences)
# Predicted - observ
sum(zero$predicted_absences) - sum(zero$actual_absences)
  # Overall reduction from
sum(zero$predicted_absences) / sum(zero$n)
# To
sum(zero$actual_absences) / sum(zero$n)
# Number of working days permanent vs temp
table(predictions$permanent_or_temporary[predictions$strategy=='Real'])
# Absences avoided among permanent workers
predictions %>% filter(strategy == 'Zero') %>% group_by(permanent_or_temporary) %>% summarise(n = n(),
                                                                                              actual_absences = sum(absent),
                                                                                              predicted_absences = sum(fit, na.rm = TRUE),
                                                                                              actual = mean(absent, na.rm = TRUE),
                                                                                              predicted = mean(fit, na.rm = TRUE)) %>%
  mutate(avoided = predicted_absences - actual_absences) %>%
  # decline due to irs
  mutate(decline = predicted - actual)
# One absence every n days
sum(zero$n) / sum(zero$avoided)
# Annual reduction in absence
mean(zero$avoided[zero$year < 2016])
# Cost per absence avoided
68984 / 4183
# Percentage of absence among permanent workers
x = predictions %>%
  filter(strategy == 'Zero') %>%
  # filter(date < '2016-01-01') %>%
  group_by(permanent_or_temporary) %>%
  summarise(absences = sum(absent),
            predicted = sum(fit)) %>%
  mutate(averted = predicted - absences) %>%
  mutate(p = averted / sum(averted))
x
# Weighted average cost
y = sum(c(25, 5) * x$p)
y
# Cost of treatment per absence
22 / 80
# Total savings per aversion
0.275 + y
# Total yearly savings
(0.275 + y) * 4183
# Total yearly savings
((0.275 + y) * 4183) - 68984
# ROI
((0.275 + y) * 4183) / 68984
# Additional max strategy
simple
47943-42131

ggplot(data = simple,
       aes(x = strategy,
           y = value)) +
  geom_bar(stat = 'identity',
           alpha = 0.6,
           color = 'black') +
  labs(y = 'Absenteeism rate',
       x = 'Strategy') +
  geom_label(aes(label = round(value, digits = 3)),
             nudge_y = -0.005)

# paste0(
#   "Of the ", nrow(simulations[simulations$strategy == 'Real',]),
#   " eligible working days in our panel, we observed ", nrow(simulations[simulations$strategy == 'Real' & simulations$absent,]), 
#   " absences. Using the above approach, we estimate that we would have observed ",
#   length(which(simulations$absent[simulations$strategy == 'Zero'])),
#   " had it not been for the firm's IRS program, for a total of ",
#   length(which(simulations$absent[simulations$strategy == 'Zero'])) - length(which(simulations$absent[simulations$strategy == 'Real'])),
#   " avoided absences. This translates to an overall reduction in absenteeism from ",
#   round(length(which(simulations$absent[simulations$strategy == 'Zero'])) / nrow(simulations %>% filter(strategy == 'Zero')) * 100, digits = 2),
#   "% (without its IRS program) to the observed ", round(length(which(final_data$absent)) / nrow(final_data) * 100, digits = 2),"%.
# The number of working-days observed among permanent workers (",
# length(which(final_data$permanent_or_temporary == 'Permanent')),") was nearly thrice that of those of temporary workers (", 
# length(which(final_data$permanent_or_temporary == 'Temporary')),
# ").") 
# 
# 
# 
# paste0(
#   "By the same token, the number of absences avoided among permanent workers (39,890) was far greater than that of temporary workers (8,972). The latter had a greater relative decline in absenteeism due to IRS (from 9.3% to 3.91%) relative to the former (from 14.2% to 11%), but the aggregate numbers are less consequential since (a) temporary workers have a lower baseline absenteeism rate and (b) they work, by definition, fewer days. In total, the IRS program is estimated to prevent one absence every 26.6 working days.
# Annually, this translates to approximately 4,076 avoided absences per year (slightly greater than the simple splicing of the overall figure into the 4-year study period, since we did not have complete data for all worker types).
# "
# )



ggplot(data = simple,
       aes(x = strategy,
           y = predicted_absences)) +
  geom_bar(stat = 'identity',
           alpha = 0.6,
           color = 'black') +
  labs(y = 'Absenteeism rate',
       x = 'Strategy') +
  geom_label(aes(label = round(predicted_absences, digits = 3)),
             nudge_y = -10000)


# Costs

x <- final_data %>%
  left_join(irs %>% dplyr::select(date, unidade, days_since))

out <- data.frame(
  strategy = c('Max',
               'Real',
               'Time-optimized',
               'Zero'),
  sprays = c(nrow(final_data) / 60,
             length(which(x$days_since == 0)),
             length(which(format(final_data$date, '%m-%d') == '12-11')),
             0
  )
) %>%
  mutate(sprays = sprays / length(unique(final_data$oracle_number)))

kable(out)
ggplot(data = out,
       aes(x = strategy,
           y = sprays)) +
  geom_bar(stat = 'identity',
           alpha = 0.6,
           color = 'black') +
  labs(y = 'Number of sprays per person',
       x = 'Strategy') 

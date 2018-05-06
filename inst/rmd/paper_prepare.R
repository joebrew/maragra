# Packages
library(tidyverse)
library(knitr)
library(Hmisc)
library(brew)
library(maragra)
library(knitr)
library(RColorBrewer)
# library(lme4)
library(lfe)

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
  weather <- maragra::weather
  workers <- maragra::workers
  
  # Define function for adding zero
  add_zero <- function (x, n) {
    x <- as.character(x)
    adders <- n - nchar(x)
    adders <- ifelse(adders < 0, 0, adders)
    for (i in 1:length(x)) {
      if (!is.na(x[i])) {
        x[i] <- paste0(paste0(rep("0", adders[i]), collapse = ""),
                       x[i], collapse = "")
      }
    }
    return(x)
  }
  
  # filter for on-site only
  
  model_data <-
    ab_panel %>%
    left_join(irs, by = c('unidade', 'date')) %>%
    mutate(days_since = days_since %/% 30) %>%
    mutate(days_since = ifelse(is.na(days_since), 'Never',
                               ifelse(days_since < 0, 'Before',
                                      ifelse(days_since >= 12, '12+', as.character(add_zero(days_since, n = 2)))))) 
  model_data <-
    model_data %>%
    mutate(absent_sick = ifelse(is.na(absent_sick), FALSE, absent_sick)) %>%
    # Add malaria incidence
    left_join(bes %>%
                dplyr::select(date, p) %>%
                dplyr::rename(incidence = p),
              by = 'date') %>%
    mutate(season = ifelse(incidence >= median(incidence),
                           'high',
                           'low')) %>%
    mutate(season = factor(season, levels = c('low', 'high'))) %>%
    # Bring in some information on workers
    left_join(workers %>%
                dplyr::select(oracle_number,
                              permanent_or_temporary,
                              department,
                              sex,
                              date_of_birth,
                              perm_id,
                              census_name_match_score) %>%
                # void the permids of anyone with a match score of greater than 0.2
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
    left_join(weather %>%
                dplyr::select(date, precipitation,
                              temp) %>%
                mutate(precipitation = ifelse(is.na(precipitation),
                                              0,
                                              precipitation))) %>%
    mutate(rainy = precipitation > 0)
  
  # Aggregate months since 
  model_data <- model_data %>%
    mutate(months_since = days_since) %>%
    mutate(months_since = as.character(months_since)) %>%
    mutate(months_since = ifelse(months_since %in% c(#'Never', 
                                                     'Before', '00', '01',
                                                     '02', '03', '04', '05', '06'),
                                 months_since,
                                 ifelse(months_since %in% c('07', '08',
                                                            '09', '10', '11', '12+'),
                                        '07+', months_since))) %>%
    # Define whether every sprayed
    ungroup %>%
    group_by(oracle_number) %>%
    mutate(ever_sprayed = length(which(unidade %in% mc$unidade)) > 0) %>%
    ungroup %>%
    # Correct the nevers
    mutate(days_since = ifelse(days_since == 'Never' & ever_sprayed, 'Before', days_since)) %>%
    mutate(months_since = as.character(months_since)) %>%
    mutate(months_since = ifelse(months_since == 'Never' & ever_sprayed, 'Before', months_since)) %>%
    # Make never and before the same
    # mutate(months_since = ifelse(months_since %in% c('Never', 'Before'), 'No IRS', months_since)) %>%
    # mutate(months_since = factor(months_since, levels = unique(c('Never', 'Before', sort(unique(months_since))))))
    # mutate(months_since = ifelse(is.na(months_since),
    #                              'No IRS',
    #                              ifelse(months_since != 'No IRS', 'IRS', 'No IRS'))) %>%
    # mutate(months_since = factor(months_since, levels = unique(c('No IRS', sort(unique(months_since)))))) %>%
    rename(on_site = ever_sprayed)
  
  model_data <- model_data %>%
    mutate(months_since = factor(months_since, levels = unique(c('Before', sort(unique(months_since)))))) 
  
  # Since ad and factory are same, keep same
  model_data$field <- ifelse(model_data$department == 'Field', 
                             'Field worker',
                             'Not field worker')
  
  
  # # Model simple
  # fit <- lm(absent ~ season * months_since + on_site, data = model_data)
  # # summary(fit)
  # 
  # # Complex model
  # fit_full <- lm(absent ~ season * months_since + on_site + sex + field + precipitation, data = model_data)
  # 
  models <- list()
  # models <- list(
  #   a = lm(absent ~ season * months_since + on_site, data = model_data),
  #   b = lm(absent ~ season * months_since + on_site + sex, data = model_data),
  #   c = lm(absent ~ season * months_since + on_site + sex + field, data = model_data),
  #   # d = lm(absent ~ season * months_since + sex + department + rainy, data = model_data),
  #   d = lm(absent ~ season * months_since + on_site + sex + field + rainy + permanent_or_temporary, data = model_data)
  # )
  
  model_data <- model_data %>% 
    # mutate(group = paste0(ifelse(on_site, 'On site', 'Off site'), ' ',
    #                          tolower(field)
    #                          ))
    mutate(group = paste0(permanent_or_temporary, ' ',
                          tolower(field)))

  # Add a month column
  model_data$month_number <- add_zero(format(model_data$date, '%m'), 2)
  
  # Get a malaria season var
  model_data <- 
    model_data %>%
    mutate(calendar_month = as.numeric(format(date, '%m'))) %>%
    mutate(calendar_year = as.numeric(format(date, '%Y'))) %>%
    mutate(malaria_year = ifelse(calendar_month < 6,
                                 paste0(calendar_year-1, '-', calendar_year),
                                 paste0(calendar_year, '-', calendar_year+1)))
  
  # Get geographic info for externality analysis
  # Get latitude / longitude into model_data
  model_data <- model_data %>%
    left_join(census %>%
                filter(!duplicated(unidade)) %>%
                dplyr::select(unidade,
                              longitude_aura,
                              latitude_aura),
              by = 'unidade') %>%
    dplyr::select(-days_since) %>%
    left_join(irs %>%
                dplyr::select(date, unidade, days_since),
              by = c('date', 'unidade'))
  
  # Create a supposed protection level based on time since IRS
  # based on Tukei: https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1652-4
  
  # # The below is not used
  # x <- c(1:6)
  # y <- (c(-3.45, -6.04, - 6.53, -0.17, -2.74, 5.27) + 27.61) / 27.61
  # lo <- loess(y~x, span = 3)$fitted
  # lo[lo > 1] <- 1
  # lo <- 1-lo
  # df <- data.frame(x,y,lo)
  # df$months_since <- add_zero(df$x, n = 2)
  # df$months_since[df$months_since == '06'] <- '06+'
  # df <- df %>%
  #   bind_rows(df[c(nrow(df), nrow(df)),] %>%
  #               mutate(months_since = c('Before', 'Never')))
  # 
  # protection <- df %>%
  #   dplyr::select(months_since, lo) %>%
  #   dplyr::rename(protection = lo)
  # 
  # model_data <- left_join(model_data,
  #                         protection,
  #                         by = 'months_since') %>%
  #   mutate(protection = ifelse(is.na(protection), 0, protection))
  # 
  
  # model_data <- model_data %>%
  #   mutate(months_since = 
  #            ifelse(months_since %in% c('06+', 'No IRS'), 'No IRS',
  #                   ifelse(is.na(months_since), 'No IRS',
  #                          'IRS')))
  
  # Remove all the nevers!
  model_data <- model_data %>%
    filter(months_since != 'Never')
  
  model_data$months_since <- factor(model_data$months_since,
                            levels = unique(c('Before', as.character(sort(unique(model_data$months_since))))))
  # model_data$p <- ifelse(model_data$months_since == 'No IRS',
  #                                    0,
  #                                    as.numeric(model_data$months_since) + 1)
  # model_data$p <- 1/model_data$p
  
  model_data$p <- ifelse(model_data$months_since %in% c('Before', 'Never'), 0, 1)
  
  # Estimate a protection factor based on the weighted protection 
  # scores of nearby houses
  library(sp)
  dates <- sort(unique(model_data$date))
  out_list <- list()
  
  weighter <- function(x){
    # x[x == 0] <- 0.01
    out <- (1 / x)#^1.2
    # out[is.infinite(out)] <- 0
    return(out)
  }
  counter <- 0
  
  bairro_loc <- apply(coordinates(maragra::bairros_maragra_bairro), 2, mean)
  for(i in 1:length(dates)){
    this_date <- dates[i]
    message(this_date, ' : ', i, ' of ', length(dates))
    this_model_data <- model_data %>% 
      filter(#!is.na(longitude_aura), 
             #!is.na(latitude_aura),
             date == this_date,
             census_name_match_score <= 0.2) %>%
      # if no geography (due to no census matching, just use average geo)
      mutate(longitude_aura = ifelse(is.na(longitude_aura), 
                                     bairro_loc[1],
                                     longitude_aura)) %>%
      
      mutate(latitude_aura = ifelse(is.na(latitude_aura), 
                                     bairro_loc[2],
                                    latitude_aura)) 
    this_model_data_spatial <- this_model_data
    coordinates(this_model_data_spatial) <- ~longitude_aura+latitude_aura
    proj4string(this_model_data_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    # Loop through each house, getting the nearby ones
    distances <- spDists(x = this_model_data_spatial)
    
    # Go through each house and get the distances, protection
    for(j in 1:nrow(this_model_data_spatial)){
      # message('---house number ', j, ' of ', nrow(this_model_data_spatial))
      sub_distances <- distances[j,]
      x <- this_model_data_spatial$protection
      w <- weighter(sub_distances)
      x <- x[is.finite(w)]
      # # Protection is 1 divided months since irs
      # p <- ifelse(this_model_data_spatial$months_since == 'No IRS',
      #             0,
      #             as.numeric(this_model_data_spatial$months_since) + 1)
      
      p <- this_model_data_spatial$p
      p[is.infinite(p)] <- 0
      p <- p[is.finite(w)]
      
      # The weight
      w <- w[is.finite(w)]
      # Number of houses within 1k
      n <- length(which(sub_distances <= 1))
      # # # The weighted protection score (average)
      # # positivity <- stats::weighted.mean(x = x,
      # #                                    w = w,
      # #                                    na.rm = TRUE)
      # # The weighted additive protection score
      # s <- sum(x[sub_distances <= 1] * w[sub_distances <= 1], na.rm = TRUE)
      
      # Product of weighted distance multipled by time since IRS
      # (this is for modeling herd protection naively)
      herdy <- p * w
      herdy <- sum(herdy, na.rm = TRUE)
      
      
      counter <- counter + 1
      id <- this_model_data_spatial$oracle_number[j]
      # message('-----', id, ': ', positivity)
      
      # Update model_data
      out_data <- data_frame(oracle_number = id,
                             date = this_date,
                             # herd = positivity,
                             n = n,
                             # s = s,
                             herdy = herdy)
      out_list[[counter]] <- out_data
    }
  }
  
  # Combine all of the time-place risk factor scores into one dataframe
  protection_df <- bind_rows(out_list)
  protection_df <- protection_df %>%
    group_by(oracle_number, date) %>%
    summarise(#herd = mean(herd, na.rm = TRUE),
              n = mean(n, na.rm = TRUE),
              #s = mean(s, na.rm = TRUE),
              herdy = mean(herdy, na.rm = TRUE))
  
  # Join to model data
  model_data <- left_join(model_data, protection_df,
                 by = c('oracle_number', 'date'))
  # model_data$herd <- model_data$s #model_data$n * model_data$herd
  model_data$herd <- NULL
  model_data$herd <- model_data$herdy
  model_data$months_since <- factor(model_data$months_since,
                                    levels = unique(c('Before', as.character(c(sort(unique(as.character(model_data$months_since))))))))
  model_data$rainy_day <- model_data$precipitation >= 0.01
  
  # Get hiv prevalence
  hiv_prevalence <- maragra::hiv_prevalence
  prevs <- model_data %>%
    group_by(longitude_aura,
             latitude_aura) %>%
    summarise(n = n()) %>%
    filter(!is.na(longitude_aura)) %>%
    mutate(x = longitude_aura,
           y = latitude_aura) %>%
    dplyr::select(-n) %>%
    ungroup
  coordinates(prevs) <- ~x+y
  x <- raster::extract(x = hiv_prevalence, y = prevs)
  prevs$hiv_prevalence <- x
  prevs$hiv_prevalence[is.na(prevs$hiv_prevalence)] <- mean(prevs$hiv_prevalence,na.rm = TRUE)
  model_data <- left_join(x = model_data,
                          y = prevs@data,
                          by = c('longitude_aura',
                                 'latitude_aura'))
  
  
  # model_data$months_since[model_data$days_since >= 183] <- 'No IRS'
  # model_data$months_since <- ifelse(model_data$months_since != 'No IRS',
  #                                   'IRS',
  #                                   'No IRS')
  save.image('~/Desktop/temp.RData')
  fe_models <- list()
  sick_models <- list()
  protection_models <- list()
  groups <- sort(unique(model_data$group))
  # library(lmerTest)
  # library(nlme)
  for (i in 1:length(groups)){
    message(i)
    this_group <- groups[i]
    these_data <- model_data %>% filter(group == this_group)
    these_data <- model_data %>% filter(group == this_group) %>%
      filter(months_since != 'Never')
    these_data$months_since <- as.character(these_data$months_since)
    
    these_data <- these_data %>%
      mutate(months_since = ifelse(months_since %in% c('Before', 'Never'), 'Before',
                                   'After'))
    these_data$months_since <- factor(these_data$months_since,
                                      levels = unique(c('Before', as.character(sort(unique(these_data$months_since))))))
    this_model <- felm(absent ~ season*months_since + rainy_day  | oracle_number + malaria_year | 0 | 0,
                       data = these_data)
    this_sick_model <- felm(absent_sick ~ season*months_since + rainy_day  | oracle_number + malaria_year| 0 | 0,
                            data = these_data)
    this_protection_model <- felm(absent ~ season*months_since + rainy_day + herd  | oracle_number + malaria_year | 0 | 0,
                       data = these_data)
    # this_protection_model <- felm(absent ~ season*months_since + precipitation + herd | oracle_number + malaria_year | 0 | 0,
    #                               data = these_data)
    fe_models[[i]] <- this_model
    sick_models[[i]] <- this_sick_model
    protection_models[[i]] <- this_protection_model
  }
  names(fe_models) <- groups
  names(sick_models) <- groups
  names(protection_models) <- groups
  
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
  
  library(ggmap)
  # if('.hdf.RData' %in% dir()){
    load('.hdf.RData')
  # } else {
  # hdf <- ggmap::get_map(location = c(lon = mar$long, lat = mar$lat), maptype = 'satellite', zoom = 14)
  #   save(hdf, file = '.hdf.RData')
  # }
  
  g4 <- ggmap::ggmap(hdf) +
    theme_map() +
    labs(title = 'iv')
  
  map_list <- list(g1,g2,g3,g4)
  
  save.image(file = 'prepared_data.RData')
  }

clean_up_model <- function(x){
  # extract coefficients
  coefs <- data.frame(coef(summary(x)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  ps <- coefs$p.z
  x = broom::tidy(x)
  for(j in 2:(ncol(x) -1)){
    x[,j] <- x[,j] * 100
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

make_models_table <- function(model_list, the_caption = "Models with worker fixed effects"){
  
  out_list <- list()
  for(i in 1:length(model_list)){
    message(i)
    this_model <- names(model_list)[i]
    the_model <- model_list[[which(names(model_list) == this_model)]]
    the_model <- clean_up_model(the_model)
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
  k <- kable(df, format = "latex", caption = the_caption, booktabs = T, longtable = TRUE) %>%
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

irs$months_since <- irs$days_since %/% 30

# Define function for making season
make_season <- function(date){
  out <- ifelse(as.numeric(format(date, '%m')) %in% c(11:12, 1:3),
                'Malaria season', 'Off season')
  out <- factor(out, levels = c('Off season', 'Malaria season'))
  return(out)
}

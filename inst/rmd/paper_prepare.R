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
                                      ifelse(days_since >= 12, '12+', as.character(add_zero(days_since, n = 2)))))) %>%
    mutate(months_since = factor(days_since, levels = unique(c('Never', 'Before', sort(unique(days_since)))))) %>%
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
    mutate(months_since = as.character(months_since)) %>%
    mutate(months_since = ifelse(months_since %in% c('Never', 'Before'),
                                 months_since,
                                 ifelse(months_since %in% c('00', '01'), '01',
                                        ifelse(months_since %in% c('02', '03', '04'),
                                               '02-04',
                                               ifelse(months_since %in% c('05', '06', '07', '08', '09'),
                                                      '05-09',
                                                      ifelse(months_since %in% c('10', '11', '12+'), '10+', months_since)))))) %>%
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
    mutate(months_since = ifelse(months_since %in% c('Never', 'Before'), 'No IRS', months_since)) %>%
    # mutate(months_since = factor(months_since, levels = unique(c('Never', 'Before', sort(unique(months_since))))))
    mutate(months_since = factor(months_since, levels = unique(c('No IRS', sort(unique(months_since)))))) %>%
    rename(on_site = ever_sprayed)
  # Since ad and factory are same, keep same
  model_data$field <- ifelse(model_data$department == 'Field', 
                             'Field worker',
                             'Not field worker')
  
  
  # Model simple
  fit <- lm(absent ~ season * months_since + on_site, data = model_data)
  # summary(fit)
  
  # Complex model
  fit_full <- lm(absent ~ season * months_since + on_site + sex + field + precipitation, data = model_data)
  
  models <- list(
    a = lm(absent ~ season * months_since + on_site, data = model_data),
    b = lm(absent ~ season * months_since + on_site + sex, data = model_data),
    c = lm(absent ~ season * months_since + on_site + sex + field, data = model_data),
    # d = lm(absent ~ season * months_since + sex + department + rainy, data = model_data),
    d = lm(absent ~ season * months_since + on_site + sex + field + rainy + permanent_or_temporary, data = model_data)
  )
  
  model_data <- model_data %>% 
    # mutate(group = paste0(ifelse(on_site, 'On site', 'Off site'), ' ',
    #                          tolower(field)
    #                          ))
    mutate(group = paste0(permanent_or_temporary, ' ',
                          tolower(field)))
  fe_models <- list()
  groups <- sort(unique(model_data$group))
  # library(lmerTest)
  
  # Add a month column
  model_data$month_number <- add_zero(format(model_data$date, '%m'), 2)
  
  # library(nlme)
  for (i in 1:length(groups)){
    message(i)
    this_group <- groups[i]
    these_data <- model_data %>% filter(group == this_group)
    this_model <- felm(absent ~ season*months_since | oracle_number | 0 | 0,
                       data = these_data)
    fe_models[[i]] <- this_model
  }
  names(fe_models) <- groups
  
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
  #   load('.hdf.RData')
  # } else {
  hdf <- ggmap::get_map(location = c(lon = mar$long, lat = mar$lat), maptype = 'satellite', zoom = 14)
  #   save(hdf, file = '.hdf.RData')
  # }
  
  g4 <- ggmap::ggmap(hdf) +
    theme_map() +
    labs(title = 'iv')
  
  map_list <- list(g1,g2,g3,g4)
  
  save.image(file = 'prepared_data.RData')
}

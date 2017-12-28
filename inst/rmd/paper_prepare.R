# Packages
library(tidyverse)
library(knitr)
library(Hmisc)
library(brew)
library(maragra)
library(knitr)
library(RColorBrewer)

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
  mutate(months_since = factor(months_since, levels = unique(c('Never', 'Before', sort(unique(months_since)))))) 
                               
# Model simple
fit <- lm(absent ~ season * months_since, data = model_data)
# summary(fit)

# Complex model
fit_full <- lm(absent ~ season * months_since + sex + department + precipitation, data = model_data)

models <- list(
  a = lm(absent ~ season * months_since, data = model_data),
  b = lm(absent ~ season * months_since + sex, data = model_data),
  c = lm(absent ~ season * months_since + sex + department, data = model_data),
  d = lm(absent ~ season * months_since + sex + department + rainy, data = model_data),
  e = lm(absent ~ season * months_since + sex + department + rainy + permanent_or_temporary, data = model_data)
)



# sick_fit <- lm(absent_sick ~ season * months_since, data = model_data)
# fit_tidy <- broom::tidy(fit)

# Full model
# model <- lm(absent ~ season * months_since + rainy + sex +
#               maragra_fabrica + education, data = model_data)
# model_tidy <- broom::tidy(model)
#
# # Predict for visualization
# model_dummy <- expand.grid(season = sort(unique(model_data$season)),
#                      months_since = sort(unique(model_data$months_since)),
#                      sex = sort(unique(model_data$sex)),
#                      rainy = sort(unique(model_data$rainy)),
#                      department = sort(unique(model_data$department)))
# predictions <- predict(model, model_dummy, se.fit = TRUE, interval = 'confidence')
# model_dummy$predicted <- predictions$fit[,1]
# model_dummy$lwr <- predictions$fit[,2]
# model_dummy$upr <- predictions$fit[,3]

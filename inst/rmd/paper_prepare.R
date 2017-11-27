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


model_data <-
  ab_panel %>%
  left_join(irs, by = c('unidade', 'date')) %>%
  mutate(days_since = ifelse(days_since > 180, '180+',
                             ifelse(days_since > 90, '090-180',
                                    ifelse(days_since > 60, '060-090',
                                           ifelse(days_since >= 0, '000-060',
                                                  ifelse(days_since < 0, 'Before', NA)))))) %>%
  mutate(days_since = ifelse(is.na(days_since), 'Never', days_since)) %>%
  mutate(days_since = factor(days_since, levels = c('Never', 'Before', '000-060', '060-090', '090-180', '180+'))) %>%
  mutate(quarter = lendable::quarter_extract(date)) %>%
  mutate(quarter = as.character(quarter)) %>%
  mutate(absent_sick = ifelse(is.na(absent_sick), FALSE, absent_sick)) %>%
    # Add malaria incidence
    left_join(bes %>%
                dplyr::select(date, p) %>%
                dplyr::rename(incidence = p),
              by = 'date') %>%
  mutate(season = ifelse(incidence >= median(incidence),
                         'high',
                         'low')) %>%
  mutate(season = factor(season, levels = c('low', 'high')))

# Model
fit <- lm(absent ~ season * days_since, data = model_data)
fit_tidy <- broom::tidy(fit)


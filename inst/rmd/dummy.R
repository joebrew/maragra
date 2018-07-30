source('paper_prepare.R')
dummy_data <- read_csv('dummy_data.csv')
dummy_data$year_month <- paste0(dummy_data$calendar_year, dummy_data$calendar_month)

# Create a model per menno's specifications
menno_models <- list()

# dummy_data <- dummy_data %>%
#   # filter(!is.na(days_since)) %>%
#   # filter(days_since >= -184, days_since <= 184) %>%
#   group_by(oracle_number, group, year_month) %>%
#   summarise(absent = length(which(absent)) / length(absent) * 100,
#             days = n(),
#             herd = mean(herd, na.rm = TRUE),
#             months_since_menno = dplyr::first(months_since_menno),
#             season = dplyr::first(season)) %>%
#   ungroup
dummy_data <- read_csv('dummy_data_collapsed.csv')
groups <- sort(unique(dummy_data$group))
# library(lmerTest)
# library(nlme)
for (i in 1:length(groups)){
  message(i)
  this_group <- groups[i]
  message(this_group)
  these_data <- dummy_data %>% filter(group == this_group)
  these_data$months_since_menno <- 
    factor(these_data$months_since_menno,
           levels = c('Before',
                      paste0(0, 1:6)))
  this_menno_model <- felm(absent ~ months_since_menno    | oracle_number + year_month | 0 | 0,
                           data = these_data)
  
  menno_models[[i]] <- this_menno_model
}
# write_csv(dummy_data, 'dummy_data_collapsed.csv')
names(menno_models) <- groups

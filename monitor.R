library(cism)
library(dplyr)
library(ggplot2)

# Get data for clinic
df <- get_data(tab = 'ECONOMICS_MAL_CORE',
               dbname = 'sapodk',
               port = 3306)

# Clean up
df$date_time <- as.POSIXct(df$`_CREATION_DATE`)

# Plot by time
by_time <-
  df %>%
  mutate(dummy = 1) %>%
  arrange(date_time) %>%
  group_by(person = DEVICEID) %>%
  mutate(cs = cumsum(dummy)) %>%
  ungroup

ggplot(data = by_time,
       aes(x = date_time,
           y = cs,
           group = DEVICEID,
           color = DEVICEID)) +
  geom_line() +
  theme_cism() +
  labs(x = 'Date-time',
       y = 'Cumulative entries',
       title = 'Maragra data entry progress',
       subtitle = 'Device IDs are unique for each worker') +
  scale_color_manual(name = 'Device ID',
                     values = c('purple', 'green'))


# Get data for malaria control
mc <- get_data(tab = 'MARAGRA_VECTOR_CONTROL_CORE',
               dbname = 'sapodk',
               port = 3306)

# Clean up
mc$date_time <- as.POSIXct(mc$`_CREATION_DATE`)
mc$date <- as.POSIXct(mc$DATA)

# Plot by time
by_time <-
  mc %>%
  mutate(dummy = 1) %>%
  arrange(date_time) %>%
  mutate(cs = cumsum(dummy)) %>%
  ungroup

ggplot(data = by_time,
       aes(x = date_time,
           y = cs)) +
  geom_line() +
  theme_cism() +
  labs(x = 'Date-time',
       y = 'Cumulative entries',
       title = 'Maragra data entry progress',
       subtitle = 'Malaria control')


# Also explore by year
by_year <- 
  mc %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year) %>%
  tally

ggplot(data = by_year,
       aes(x = year,
           y = n)) +
  geom_bar(stat = 'identity',
           fill = 'darkgreen',
           alpha = 0.6) +
  theme_cism() +
  labs(x = 'Year',
       y = 'Records entered',
       title = 'Progress in malaria control data entry')
  
# Write a csv
readr::write_csv(mc, '~/Desktop/eduardo.csv')

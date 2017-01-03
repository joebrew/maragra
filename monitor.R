library(cism)
library(dplyr)
library(ggplot2)

# Get data
df <- get_data(tab = 'ECONOMICS_MAL_CORE',
               dbname = 'sapodk',
               port = 4706)

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

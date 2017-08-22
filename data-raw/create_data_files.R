library(devtools)
library(readr)
library(tidyr)
library(dplyr)
library(cism)
library(dplyr)
library(ggplot2)
library(readxl)

# Initiate the raw-data into package session
devtools::use_data_raw()

# READ DATA ###################################

# Read in absenteeism data
# ab <- readxl::read_excel('HRS - Leave Applications.xls') # too large
ab <- readr::read_csv('HRS - Leave Applications.csv')
ab2 <- readr::read_csv('HRS - Leave Applications - Sheet 2.csv')
ab <- bind_rows(ab, ab2)
rm(ab2)

# Read in clinic data
clinic <- get_data(tab = 'ECONOMICS_MAL_CORE',
               dbname = 'sapodk')

# Read in aggregate level clinic data
clinic_agg <- read_csv('maragra_monthly_data.csv')

# Read in malaria control data
mc <- get_data(tab = 'MARAGRA_VECTOR_CONTROL_CORE',
               dbname = 'sapodk')

# Read in workers data
workers <- readxl::read_excel('All PERMANENT & NOM PERMANENT EE TO USE IN COMPLEMENT REPORT.xls',
                              skip = 2)

# Save a backup
save.image(paste0('../backups/', Sys.Date(), '.RData'))

# CLEAN DATA ###################################

# ab ########
# Get a date column
ab$date <- as.Date(ab$`Date Captured`, format = '%d-%b-%Y')

# Clean up column names
names(ab) <- tolower(gsub(' ', '_', names(ab)))

# clinic ######
# Clean up column names
names(clinic) <- tolower(gsub(' ', '_', names(clinic)))

# Create a name column
person_name <- iconv(clinic$b_group_full_name,"WINDOWS-1252","UTF-8")
person_name <- tolower(person_name)
person_name <- gsub(' _pf| -pf| -po| _po|_pf|pf|-pf|4g| _pg| _f| _|_po|3,pm| _f| _pm|_po|_pt|_pg|_f|_f,g| _f| _g| _pm|_pm', '', person_name)
clinic$name <- toupper(person_name)

# Clean up dates
clinic$creation_date <- as.Date(clinic$`_creation_date`)
clinic$date <- as.Date(clinic$b_group_date)

# Get severity
clinic$severity <- clinic$b_group_malaria_severity

# Keep only relevant columns
clinic <-
  clinic %>%
  dplyr::select(date, name, severity, creation_date)

# clinic_agg ######
# Create date column
clinic_agg$date <- as.Date(paste0(clinic_agg$year, '-', clinic_agg$month, '-', 15))

# Create a percent postiive column
clinic_agg$percent_positive <- clinic_agg$positive / clinic_agg$tested * 100

# Arrange by date
clinic_agg <- clinic_agg %>% arrange(date)

# Get rid of any unknowns
clinic_agg <- clinic_agg %>% filter(!is.na(tested))


# mc #######
# Clean up
mc$date_time <- as.POSIXct(mc$`_CREATION_DATE`)
mc$date <- as.POSIXct(mc$DATA)

# workers ######


# SAVE DATA #####################################
# (for use in package, once cleaning is done)
devtools::use_data(ab,
                   overwrite = TRUE)
devtools::use_data(clinic,
                   overwrite = TRUE)
devtools::use_data(clinic_agg,
                   overwrite = TRUE)
devtools::use_data(mc,
                   overwrite = TRUE)
devtools::use_data(workers,
                   overwrite = TRUE)

# Old plots 
# x <- ab %>%
#   mutate(year_month = format(date, '%Y-%m')) %>%
#   mutate(month = as.Date(paste0(year_month, '-01'))) %>%
#   group_by(date = month) %>%
#   tally
# 
# ggplot(data = x,
#        aes(x = date,
#            y = n)) +
#   geom_line() 
# 
# 
# 
# # Plot by time
# by_time <-
#   mc %>%
#   mutate(dummy = 1) %>%
#   arrange(date_time) %>%
#   mutate(cs = cumsum(dummy)) %>%
#   ungroup
# 
# ggplot(data = by_time,
#        aes(x = date_time,
#            y = cs)) +
#   geom_line() +
#   theme_cism() +
#   labs(x = 'Date-time',
#        y = 'Cumulative entries',
#        title = 'Maragra data entry progress',
#        subtitle = 'Malaria control')
# 
# 
# # Also explore by year
# by_year <- 
#   mc %>%
#   mutate(year = as.numeric(format(date, '%Y'))) %>%
#   group_by(year) %>%
#   tally
# 
# ggplot(data = by_year,
#        aes(x = year,
#            y = n)) +
#   geom_bar(stat = 'identity',
#            fill = 'darkgreen',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(x = 'Year',
#        y = 'Records entered',
#        title = 'Progress in malaria control data entry')

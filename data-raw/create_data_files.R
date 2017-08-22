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

# ab ####################################################

# Read in absenteeism data
# ab <- readxl::read_excel('HRS - Leave Applications.xls') # too large
ab <- readr::read_csv('HRS - Leave Applications.csv')
ab2 <- readr::read_csv('HRS - Leave Applications - Sheet 2.csv')
ab <- bind_rows(ab, ab2)
rm(ab2)

# Get a date column
ab$date <- as.Date(ab$`Date Captured`, format = '%d-%b-%Y')

# Clean up column names
names(ab) <- tolower(gsub(' ', '_', names(ab)))

# Save for use in package
devtools::use_data(ab,
                   overwrite = TRUE)

# clinic ##################################################

# Read in clinic data
if('clinic.RData' %in% dir()){
  load('clinic.RData')
} else {
  clinic <- get_data(tab = 'ECONOMICS_MAL_CORE',
                     dbname = 'sapodk')
  save(clinic,
       file = 'clinic.RData')
}
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

# Save for use in package
devtools::use_data(clinic,
                   overwrite = TRUE)

# clinic_agg ##################################################

# Read in aggregate level clinic data
clinic_agg <- read_csv('maragra_monthly_data.csv')
# Create date column
clinic_agg$date <- as.Date(paste0(clinic_agg$year, '-', clinic_agg$month, '-', 15))

# Create a percent postiive column
clinic_agg$percent_positive <- clinic_agg$positive / clinic_agg$tested * 100

# Arrange by date
clinic_agg <- clinic_agg %>% arrange(date)

# Get rid of any unknowns
clinic_agg <- clinic_agg %>% filter(!is.na(tested))

# Save for use in package
devtools::use_data(clinic_agg,
                   overwrite = TRUE)



# mc ###################################################

# Read in malaria control data
if('mc.RData' %in% dir()){
  load('mc.RData')
} else {
  mc <- get_data(tab = 'MARAGRA_VECTOR_CONTROL_CORE',
                 dbname = 'sapodk')
  save(mc, file = 'mc.RData')
}
# Clean up
mc$date_time <- as.POSIXct(mc$`_CREATION_DATE`)
mc$date <- as.POSIXct(mc$DATA)

# Save for use in package
devtools::use_data(mc,
                   overwrite = TRUE)


# workers ##################################################
# Read in workers data
workers <- readxl::read_excel('All PERMANENT & NOM PERMANENT EE TO USE IN COMPLEMENT REPORT.xls',
                              skip = 2)

# Save for use in package
devtools::use_data(workers,
                   overwrite = TRUE)

##################################################
# Save a backup
save.image(paste0('../backups/', Sys.Date(), '.RData'))






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

# Define some helper functions

# Generate date columns
make_date_columns <- function(df){
  if(!'date' %in% names(df)){
    stop('Must have a column named date')
  }
  if(class(df$date) != 'Date'){
    stop('date column must be of class Date')
  }
  if('month' %in% names(df) | 'year' %in% names(df) |
     'dow' %in% names(df) | 'day' %in% names(df) |
     'day_number' %in% names(df) | 'year_month' %in% names(df)){
    stop('Cannot have already have columns named month, year, dow, day, day_number, or year_month')
  }
  df <- df %>%
    mutate(month = as.numeric(format(date, '%m')),
           year = as.numeric(format(date, '%Y')),
           dow = weekdays(date),
           day = as.numeric(format(date, '%d')),
           day_number = as.numeric(format(date, '%j'))) %>%
    mutate(year_month = paste0(year, '-', month))
  return(df)
}


# ab ####################################################

# Read in absenteeism data
# ab <- readxl::read_excel('HRS - Leave Applications.xls') # too large
ab <- readr::read_csv('HRS - Leave Applications.csv')
ab2 <- readr::read_csv('HRS - Leave Applications - Sheet 2.csv')
ab <- bind_rows(ab, ab2)
rm(ab2)

# Clean up column names
names(ab) <- tolower(gsub(' ', '_', names(ab)))

# Format dates
format_date <- function(x){as.Date(x, '%d-%b-%Y')}
ab <- ab %>%
  mutate(date_captured = format_date(date_captured),
         leave_from_date = format_date(leave_from_date),
         leave_to_date = format_date(leave_to_date),
         date_approved = format_date(date_approved),
         date_captured = format_date(date_captured),
         date_signed_leave_form_rec = format_date(date_signed_leave_form_rec))

# Keep only relevant columns
ab <-
  ab %>%
  dplyr::select(-update_indicator,
                -leave_sequence_number,
                -leave_reason,
                -date_captured,
                -leave_form_number,
                -paid_unpaid_indicator,
                -oracle_number_approved_by,
                -oracle_number_denied_by,
                -date_denied,
                -date_approved,
                -signed_leave_form_rec_ind,
                -date_signed_leave_form_rec,
                -bulk_print_indicator,
                -fraction_leave_date,
                -oracle_number_received_by,
                -captured_by_hospital_indicator,
                -reason_denied)

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
  dplyr::select(date, name, severity)

# Make date columns
clinic <- clinic %>%
  make_date_columns()

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
mc$date <- as.Date(mc$date)

# Clean up column names
names(mc) <- tolower(names(mc))

# Keep only useful columns
mc <- mc %>%
  dplyr::select(date,
                insecticida,
                casas_cobertas,
                pulverizados,
                meta_instance_name,
                unidade,
                casas_total)

# Generate other date columns
mc <- mc %>%
  make_date_columns()

# Save for use in package
devtools::use_data(mc,
                   overwrite = TRUE)


# workers ##################################################
# Read in workers data
workers <- readxl::read_excel('All PERMANENT & NOM PERMANENT EE TO USE IN COMPLEMENT REPORT.xls',
                              skip = 2)

# Clean up column names
names(workers) <- tolower(gsub(' ', '_', names(workers)))

# Combine all addresses
leave_out_na <- function(x){ifelse(is.na(x), '', x)}
workers$address <-
  paste0(leave_out_na(workers$address_1),
         ' ',
         leave_out_na(workers$address_2),
         ' ',
         leave_out_na(workers$address_3))
workers$address <- trimws(workers$address, which = 'both')

# Clean up date formats
workers <-
  workers %>%
  mutate(date_of_birth = as.Date(date_of_birth),
         contract_start_date = as.Date(contract_start_date),
         contract_end_date = as.Date(contract_end_date),
         company_entry_date = as.Date(company_entry_date))

# Need to clean job_title and address through standardization !!!

# Save for use in package
devtools::use_data(workers,
                   overwrite = TRUE)


##### COMBINED

# We need to get worker eligible days into an expanded (panel) format
expandify <- function(oracle_number = 'abc',
                      start = as.Date('2015-01-01'),
                      end = as.Date('2015-01-15')){
  if(is.na(start) | is.na(end)){
    out <- data_frame(oracle_number = as.character(rep(NA, 0)),
                      date = as.Date(rep(NA, 0)))
  } else {
    dates <- seq(start,
                 end,
                 by = 1)
    out <- data_frame(oracle_number,
                      date = dates)
  }
  return(out)
}

# Go through each worker and get the eligible working days
if('eligible_working_days.RData' %in% dir()){
  load('eligible_working_days.RData')
} else {
  eligible_working_days <- list()
  nrw <- nrow(workers)
  for(i in 1:nrw){
    message(i, ' of ', nrw)
    worker_type <- workers$employee_indicator_type[i]
    if(worker_type == 'C'){
      start_date <- workers$contract_start_date[i]
      end_date <- workers$contract_end_date[i]
    } else {
      start_date <- workers$company_entry_date[i]
      end_date <- as.Date('2017-03-31')
    }
    eligible_working_days[[i]] <- 
      expandify(oracle_number = workers$oracle_number[i],
                start = start_date,
                end = end_date)
  }
  
  # Bind all together
  eligible_working_days <- bind_rows(eligible_working_days)
  save(eligible_working_days,
       file = 'eligible_working_days.RData')
}

# Expand absences
if('ab_expanded.RData' %in% dir()){
  load('ab_expanded.RData')
} else {
  ab_expanded <- list()
  nra <- nrow(ab)
  for (i in 1:nra){
    message(i, ' of ', nra)
    this_oracle_number <- ab$oracle_number[i]
    sub_data <- ab[i,]
    x <- expandify(oracle_number = this_oracle_number,
                   start = sub_data$leave_from_date,
                   end = sub_data$leave_to_date)
    x <- left_join(x,
                   y = sub_data %>%
                     dplyr::select(oracle_number,
                                   leave_type,
                                   leave_taken),
                   by = 'oracle_number')
    ab_expanded[[i]] <- x
  }
  ab_expanded <- bind_rows(ab_expanded)
  ab_expanded$absent <- TRUE
  save(ab_expanded,
       file = 'ab_expanded.RData')
}

# Join with absences
ab_panel <- left_join(x = eligible_working_days,
                    y = ab_expanded,
                    by = c('oracle_number', 'date')) %>%
  mutate(absent = ifelse(is.na(absent), FALSE, absent))

# Define whether sick or not
ab_panel$absent_sick <-
  ifelse(ab_panel$absent &
           ab_panel$leave_type == 'SIC', TRUE,
         ifelse(!ab_panel$absent, NA,
                FALSE))

devtools::use_data(ab_panel,
                   overwrite = TRUE)

##################################################
# Save a backup
save.image(paste0('../backups/', Sys.Date(), '.RData'))






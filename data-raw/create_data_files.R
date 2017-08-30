library(devtools)
library(readr)
library(tidyr)
library(dplyr)
library(cism)
library(dplyr)
library(ggplot2)
library(readxl)
library(rgdal)
library(sp)

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

# Keep only post 2013
ab <- ab %>%
  filter(leave_from_date >= '2013-01-01')

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

# Clean up the insecticide column
mc <-
  mc %>%
  mutate(insecticida = toupper(insecticida)) %>%
  mutate(insecticida = ifelse(grepl('DDT', insecticida),
                              'DDT',
                              ifelse(grepl('ACT|ACY|AVT', insecticida), 'ACT', NA)))

# Add a 0 to the unidade (since they should all be in maciana)
mc$unidade <- paste0('0', mc$unidade)


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

# Need to clean job_title through standardization !!!




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

# Get rid of the period before which we had any absences
ab_panel <- ab_panel %>%
  dplyr::filter(date >= min(ab$leave_from_date),
                date <= max(ab$leave_to_date))

# Get rid of everything before 2013, since it appears unreliable
ab_panel <-
  ab_panel %>%
  dplyr::filter(date >= '2013-01-01') %>%
  # and anything in 2017, which also appears untrustable
  dplyr::filter(date <= '2016-12-31')



# DSS bairros data
library(rgdal)
bairros <- readOGR('bairros_spatial', 'DSS_MANHICA')



# Census data ###############################
# The below uses the same approach we used for maltem schools
if(!'census_done.RData' %in% dir('census_data')){
  #### MANHICA CENSUS
  # Get data from dssodk
  if('manhica_census_data.RData' %in% dir('census_data')){
    load('census_data/manhica_census_data.RData')
  } else {
    # Get manhica census data
    HOUSEHOLD_ECONOMICS_CORE <-
      cism::get_data(tab = 'HOUSEHOLD_ECONOMICS_CORE',
                     dbname = 'dssodk')
    INDIVIDUAL_DETAILS_CORE <-
      cism::get_data(tab = 'INDIVIDUAL_DETAILS_CORE',
                     dbname = 'dssodk')
    LOCATION_DETAILS_CORE <-
      cism::get_data(tab = 'LOCATION_DETAILS_CORE',
                     dbname = 'dssodk')
    location <-
      cism::get_data(tab = 'location',
                     dbname = 'openhds')
    save(HOUSEHOLD_ECONOMICS_CORE,
         INDIVIDUAL_DETAILS_CORE,
         LOCATION_DETAILS_CORE,
         location,
         file = 'census_data/manhica_census_data.RData')
  }

  # Clean up manhica data ---------------

  # Remove weirdness from column names
  names(LOCATION_DETAILS_CORE) <-
    gsub('SEC1_|SEC2_|SEC3_|SEC4_|SEC5_',
         '',
         names(LOCATION_DETAILS_CORE))

  # Get smaller data
  LOCATION_DETAILS_CORE <-
    LOCATION_DETAILS_CORE %>%
    dplyr::select(LOCATION_ID,
                  COVERAGE_MATERIAL,
                  FLOOR_MATERIAL,
                  HAS_KITCHEN,
                  ILUMINATION_FUEL,
                  IS_KITCHEN_INSIDE,
                  KITCHEN_FUEL,
                  KITCHEN_HAS_COVERAGE,
                  LATRINE_TYPE,
                  NR_CONSTRUCTIONS,
                  NR_HOUSE_DIVISIONS,
                  WALL_MATERIAL,
                  WATER_SOURCE) %>%
    filter(!duplicated(LOCATION_ID))
  INDIVIDUAL_DETAILS_CORE <-
    INDIVIDUAL_DETAILS_CORE %>%
    dplyr::select(LOCATION_ID,
                  PERM_ID,
                  DOB,
                  NAME,
                  GENDER,
                  EDUCATION,
                  OCUPATION)
  HOUSEHOLD_ECONOMICS_CORE <-
    HOUSEHOLD_ECONOMICS_CORE %>%
    dplyr::select(LOCATION_ID,
                  HAS_FREEZER,
                  HAS_GLACIER,
                  HAS_TV,
                  NR_OF_BIKE,
                  NR_OF_CAR,
                  NR_OF_CATTLE,
                  NR_OF_CHICKENS,
                  NR_OF_DUCK,
                  NR_OF_GOAT,
                  NR_OF_MOTO,
                  NR_OF_PIGS,
                  NR_OF_TRACTOR,
                  HAS_TRACTOR) %>%
    filter(!duplicated(LOCATION_ID))


  # Get dictionary for dssodk
  dictionary <-
    cism::get_dssodk_dictionary()
  # Remove anything with "nr_of"
  dictionary <-
    dictionary %>%
    filter(!grepl('nr_of', tolower(variable)))

  # Apply dictionary to LOCATION_DETAILS_CORE
  LOCATION_DETAILS_CORE <- data.frame(LOCATION_DETAILS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'LOCATION_DETAILS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(LOCATION_DETAILS_CORE)){
    message(j)
    this_column <- names(LOCATION_DETAILS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      LOCATION_DETAILS_CORE[,this_column] <-
        ifelse(as.character(LOCATION_DETAILS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               LOCATION_DETAILS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      LOCATION_DETAILS_CORE$old <- LOCATION_DETAILS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) !=
         class(LOCATION_DETAILS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        LOCATION_DETAILS_CORE$old <- as.character(LOCATION_DETAILS_CORE$old)
      }
      LOCATION_DETAILS_CORE <-
        LOCATION_DETAILS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      LOCATION_DETAILS_CORE[,this_column] <-
        LOCATION_DETAILS_CORE$answer_eng
      LOCATION_DETAILS_CORE$old <- NULL
      LOCATION_DETAILS_CORE$answer_eng <- NULL
    }
  }

  # Apply dictionary to INDIVIDUAL_DETAILS_CORE
  INDIVIDUAL_DETAILS_CORE <- data.frame(INDIVIDUAL_DETAILS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'INDIVIDUAL_DETAILS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(INDIVIDUAL_DETAILS_CORE)){
    message(j)
    this_column <- names(INDIVIDUAL_DETAILS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      INDIVIDUAL_DETAILS_CORE[,this_column] <-
        ifelse(as.character(INDIVIDUAL_DETAILS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               INDIVIDUAL_DETAILS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      INDIVIDUAL_DETAILS_CORE$old <- INDIVIDUAL_DETAILS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) !=
         class(INDIVIDUAL_DETAILS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        INDIVIDUAL_DETAILS_CORE$old <- as.character(INDIVIDUAL_DETAILS_CORE$old)
      }
      INDIVIDUAL_DETAILS_CORE <-
        INDIVIDUAL_DETAILS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      INDIVIDUAL_DETAILS_CORE[,this_column] <-
        INDIVIDUAL_DETAILS_CORE$answer_eng
      INDIVIDUAL_DETAILS_CORE$old <- NULL
      INDIVIDUAL_DETAILS_CORE$answer_eng <- NULL
    }
  }

  # Apply dictionary to HOUSEHOLD_ECONOMICS_CORE
  HOUSEHOLD_ECONOMICS_CORE <- data.frame(HOUSEHOLD_ECONOMICS_CORE)
  small_dictionary <- dictionary %>%
    filter(table == 'HOUSEHOLD_ECONOMICS_CORE',
           db == 'dssodk')
  for(j in 1:ncol(HOUSEHOLD_ECONOMICS_CORE)){
    message(j)
    this_column <- names(HOUSEHOLD_ECONOMICS_CORE)[j]
    if(grepl('nr_of', tolower(this_column))){
      HOUSEHOLD_ECONOMICS_CORE[,this_column] <-
        ifelse(as.character(HOUSEHOLD_ECONOMICS_CORE[,this_column]) %in% c('88', '99'),
               NA,
               HOUSEHOLD_ECONOMICS_CORE[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      HOUSEHOLD_ECONOMICS_CORE$old <- HOUSEHOLD_ECONOMICS_CORE[,this_column]
      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) !=
         class(HOUSEHOLD_ECONOMICS_CORE$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        HOUSEHOLD_ECONOMICS_CORE$old <- as.character(HOUSEHOLD_ECONOMICS_CORE$old)
      }
      HOUSEHOLD_ECONOMICS_CORE <-
        HOUSEHOLD_ECONOMICS_CORE %>%
        left_join(sub_dictionary,
                  by = 'old')
      HOUSEHOLD_ECONOMICS_CORE[,this_column] <-
        HOUSEHOLD_ECONOMICS_CORE$answer_eng
      HOUSEHOLD_ECONOMICS_CORE$old <- NULL
      HOUSEHOLD_ECONOMICS_CORE$answer_eng <- NULL
    }
  }

  # Join individual with location
  manhica_people <-
    left_join(x = INDIVIDUAL_DETAILS_CORE,
              y = location %>%
                dplyr::select(extId,
                              longitude,
                              latitude),
              by = c('LOCATION_ID' = 'extId'))
  # Bring in information from the location_details
  manhica_people <-
    left_join(x = manhica_people,
              y = LOCATION_DETAILS_CORE,
              by = 'LOCATION_ID')
  # Bring in information form household economics
  manhica_people <-
    left_join(x = manhica_people,
              y = HOUSEHOLD_ECONOMICS_CORE,
              by = 'LOCATION_ID')
  # Specify the source
  manhica_people$district <- 'Manhiça'

  # remove some extra objects
  rm(HOUSEHOLD_ECONOMICS_CORE,
     INDIVIDUAL_DETAILS_CORE,
     location,
     LOCATION_DETAILS_CORE,
     small_dictionary,
     sub_dictionary,
     j,
     this_column,
     dictionary)

  # Magude census #################
  if('2016-12-07_HOUSEHOLD.RData' %in% dir('census_data')){
    load('census_data/2016-12-07_HOUSEHOLD.RData')
  } else {
    HOUSEHOLD <- get_data(dbname = 'MALTEM',
                          tab = 'HOUSEHOLD')
    save(HOUSEHOLD,
         file = 'census_data/2016-12-07_HOUSEHOLD.RData')
  }
  if('2016-12-07_MEMBER.RData' %in% dir('census_data')){
    load('census_data/2016-12-07_MEMBER.RData')
  } else {
    MEMBER <- get_data(dbname = 'MALTEM',
                       tab = 'MEMBER')
    save(MEMBER,
         file = 'census_data/2016-12-07_MEMBER.RData')
  }

  # Join member and household
  magude <- left_join(x = MEMBER,
                      y = HOUSEHOLD,
                      by = c('_PARENT_AURI'='_URI'))

  # Get a dictionary for translating responses
  dictionary <- cism::get_maltem_dictionary()

  # Apply the dictionary to magude
  magude <- data.frame(magude)
  small_dictionary <- dictionary %>%
    filter(db == 'MALTEM')
  for(j in 1:ncol(magude)){
    message(j)
    this_column <- names(magude)[j]
    if(grepl('nr_of', tolower(this_column))){
      magude[,this_column] <-
        ifelse(as.character(magude[,this_column]) %in% c('88', '90', '99'),
               NA,
               magude[,this_column])
    }
    if(this_column %in% small_dictionary$variable){
      # Get the dictionary just for the variable in question
      sub_dictionary <- small_dictionary %>%
        filter(variable == this_column) %>%
        dplyr::select(old, answer_eng)
      # Replace the variable
      magude$old <- magude[,this_column]

      # If not the same type, coerce to character before join
      if(class(sub_dictionary$old) !=
         class(magude$old)){
        sub_dictionary$old <- as.character(sub_dictionary$old)
        magude$old <- as.character(magude$old)
      }

      magude <-
        magude %>%
        left_join(sub_dictionary,
                  by = 'old')
      magude[,this_column] <-
        magude$answer_eng
      magude$old <- NULL
      magude$answer_eng <- NULL
    }
  }

  # Get geographic coordinates
  magude <-
    magude %>%
    mutate(latitude = as.numeric(as.character(HOUSEHOLD_HEAD_GPS_LAT)),
           longitude = as.numeric(as.character(HOUSEHOLD_HEAD_GPS_LNG)))

  # Define the source
  magude$district <- 'Magude'

  # Rename perm id
  magude <-
    magude %>%
    rename(PERM_ID = PERM_ID_MEMBER)

  # Rename those columns to match the ones in manhica census

  url_of_matcher <-
    'https://docs.google.com/spreadsheets/d/1bOBq0scJv-id656YUIZAtlWaqObcHFoH6Kmz3gQpj5E/edit#gid=1923569214'
  matcher <- gsheet::gsheet2tbl(url_of_matcher)

  # Loop through each name in magude and manhica census and standardize
  for (j in 1:ncol(manhica_people)){
    this_column <- names(manhica_people)[j]
    if(this_column %in% matcher$manhica){
      new_name <- matcher$final[matcher$manhica == this_column]
      names(manhica_people)[j] <- new_name
    }
  }
  for (j in 1:ncol(magude)){
    this_column <- names(magude)[j]
    if(this_column %in% matcher$magude){
      new_name <- matcher$final[matcher$magude == this_column]
      names(magude)[j] <- new_name
    }
  }

  # Get birth day
  magude$dob <- as.Date(magude$BIRTH_MEMBER)

  # Rename a few colunns in magude
  magude <-
    magude %>%
    rename(name = MEMBER_NAME,
           sex = MEMBER_GENDER) %>%
    mutate(sex = ifelse(sex == 1, 'M',
                        ifelse(sex == 2, 'F',
                               NA)))

  # rename a few more columns
  manhica_people <- manhica_people %>%
    mutate(dob = as.Date(DOB)) %>%
    mutate(sex = ifelse(GENDER == 'male', 'M',
                        ifelse(GENDER == 'female', 'F', NA))) %>%
    rename(name = NAME) %>%
    mutate(latitude = as.numeric(as.character(latitude)),
           longitude = as.numeric(as.character(longitude)))

  # Lowercase permid in both places
  manhica_people$perm_id <- manhica_people$PERM_ID
  magude$perm_id <- magude$PERM_ID
  # Define which columns to keep
  keep <- c('name',
            'perm_id',
            'sex',
            'dob',
            'district',
            'longitude',
            'latitude',
            matcher$final)

  # Keep only those columns
  manhica_people <-
    manhica_people[,keep]
  magude <- magude[,keep]

  # Make sure types match
  for (j in 1:ncol(magude)){
    if(class(magude[,j]) !=
       class(manhica_people[,j])){
      magude[,j] <- as.character(magude[,j])
      manhica_people[,j] <- as.character(manhica_people[,j])
    }
  }


  # Combine manhica and magude into one
  census <- bind_rows(manhica_people, magude)
  census <-
    census %>%
    filter(!duplicated(name, dob))

  # Fix classes
  census <- census %>%
    mutate(n_bikes = as.numeric(n_bikes),
           n_cars = as.numeric(n_cars),
           n_chickens = as.numeric(n_chickens),
           n_cows = as.numeric(n_cows),
           n_ducks = as.numeric(n_ducks),
           n_goats = as.numeric(n_goats),
           n_moto = as.numeric(n_moto),
           n_house_divisions = as.numeric(n_house_divisions),
           n_constructions = as.numeric(n_constructions),
           n_pigs = as.numeric(n_pigs))

  # Fix some more oddities
  census <-
    census %>%
    mutate(n_bikes = ifelse(n_bikes >= 10, NA, n_bikes),
           n_cars = ifelse(n_cars >= 10, NA, n_cars),
           n_chickens = ifelse(n_chickens >= 87, NA, n_chickens),
           n_cows = ifelse(n_cows >= 87, NA, n_cows),
           n_ducks = ifelse(n_ducks >= 87, NA, n_ducks),
           n_goats = ifelse(n_goats >= 87, NA, n_goats),
           n_moto = ifelse(n_moto >= 10, NA, n_moto),
           n_house_divisions = ifelse(n_house_divisions >= 20,
                                      NA,
                                      n_house_divisions),
           n_constructions = ifelse(n_constructions >= 30,
                                    NA,
                                    n_constructions),
           n_pigs = ifelse(n_pigs >= 87,
                           NA,
                           n_pigs))

  # Remove unecessary objects
  rm(dictionary,
     HOUSEHOLD,
     magude,
     manhica_people,
     matcher,
     MEMBER,
     small_dictionary,
     sub_dictionary,
     keep,
     url_of_matcher)

  save(census,
       file = 'census_data/census_done.RData')
} else {
  load('census_data/census_done.RData')
}

# Keep only relevant columns (since most census info we'll get from openhds only)
demografia <- census %>% 
  dplyr::select(-unidade,
                -name)


# Here we get census data per the approach in ilha josina
# (more accurate)

# Get data from openhds
if('open_hds_data.RData' %in% dir('census_data')){
  load('census_data/open_hds_data.RData')
} else {
  membership <- 
    cism::get_data(tab = 'membership',
                   dbname = 'openhds')
  individual <- 
    cism::get_data(tab = 'individual',
                   dbname = 'openhds')
  location <- 
    cism::get_data(tab = 'location',
                   dbname = 'openhds')
  residency <-
    cism::get_data(tab = 'residency',
                   dbname = 'openhds')
  VISIT_REGISTRATION_CORE <-
    cism::get_data(tab = 'VISIT_REGISTRATION_CORE',
                   dbname = 'dssodk')
  
  save(membership,
       individual,
       location,
       residency,
       VISIT_REGISTRATION_CORE,
       file = '../data/open_hds_data.RData')
}
# Clean up -----------------------------------

# Remove the extra characters in invdividual.extId
individual$extId <- substr(individual$extId,
                           start = 1,
                           stop = 9)

# Remove those with hh in permid, and by permid we mean lastname
individual <- individual %>%
  filter(!grepl('hh', tolower(lastName)))

# Make data objects
residency <- residency %>%
  mutate(startDate = as.Date(startDate),
         endDate = as.Date(endDate, origin = '1970-01-01'))
individual$dob <- as.Date(individual$dob)

# We're going to snapshot on. So, remove
# those observations that come before/after, etc.
snap_shot <- as.Date('2016-01-01')
residency <- residency %>%
  mutate(endDate = ifelse(is.na(endDate), snap_shot, endDate)) %>%
  filter(startDate <= snap_shot,
         endDate >= snap_shot)

# Keep only those people as of the snap_shot date
people <- residency %>%
  dplyr::select(individual_uuid,
                location_uuid) %>%
  left_join(individual %>%
              dplyr::select(#extId,
                uuid,
                dob,
                firstName,
                gender,
                lastName,
                middleName,
                gender),
            by = c('individual_uuid' = 'uuid')) %>%
  left_join(location %>%
              dplyr::select(extId,
                            uuid,
                            latitude,
                            locationName,
                            longitude),
            by = c('location_uuid' = 'uuid'))

people$longitude <- as.numeric(as.character(people$longitude))
people$latitude <- as.numeric(as.character(people$latitude))

# Join to visit registration core from dssodk
people <- people %>%
  left_join(VISIT_REGISTRATION_CORE %>%
              dplyr::select(LOCATION_NAME,
                            COORDINATES_LAT,
                            COORDINATES_LNG) %>%
              filter(!is.na(LOCATION_NAME),
                     !is.na(COORDINATES_LAT),
                     !is.na(COORDINATES_LNG)) %>%
              filter(!duplicated(LOCATION_NAME)),
            by = c('locationName' = 'LOCATION_NAME'))
people$latitude <- 
  ifelse(is.na(people$latitude), people$COORDINATES_LAT, people$latitude)
people$longitude <- 
  ifelse(is.na(people$longitude), people$COORDINATES_LNG, people$longitude)

# Get an age
people <- people %>%
  mutate(age_years = as.numeric(snap_shot - dob) / 365.25) 

# Fix the last name / permid naming issue
people <- people %>%
  rename(perm_id = lastName)

# Get a household id
people <- people %>%
  mutate(household_id = substr(x = perm_id, 
                               start = 1, 
                               stop = 8))

# Get a maragra-ready unidade
people$unidade <- gsub('-', '', people$household_id)

# Remove duplicates
people <- people %>%
  filter(!duplicated(perm_id))

# Fix the name
people$name <- people$firstName

# Get a copy of the full census
census <- people

# Clean up encoding
census$name <- iconv(census$name,"WINDOWS-1252","UTF-8")

# Keep only relevant columns
census <- census %>% 
  rename(sex = gender) %>%
  dplyr::select(dob, 
                name,
                perm_id,
                longitude,
                latitude,
                age_years,
                unidade,
                sex)

# Join to demographic info
# first changing lat/lon to not cause merge problems
demografia <-
  demografia %>%
  rename(x = longitude,
         y = latitude)
demografia <- demografia %>%
  dplyr::filter(!duplicated(perm_id))
census <- census %>%
  dplyr::filter(!duplicated(perm_id))
census <- full_join(x = census,
                    y = demografia)
rm(demografia)
# Clean up lat /long
census <- census %>%
  mutate(longitude = ifelse(is.na(longitude), x, longitude),
         latitude = ifelse(is.na(latitude), y, latitude)) %>%
  dplyr::select(-x, -y, -district)

# Define which are in maragra bairro and fabrica
census <-
  census %>%
  mutate(maragra_bairro = substr(perm_id, 1, 4) %in%
           c(paste0('0', 601:609),
             paste0('0', 701:709),
             1001:1003)) %>%
  mutate(maragra_fabrica = substr(perm_id, 1, 4) %in%
           as.character(1001:1003))


# Correct the unidades when incorrectly digitized
for (i in 1:nrow(mc)){
  old_unidade <- mc$unidade[i]
  new_unidade <- maragra::correct_unidade(unidade = old_unidade,
                                          unidades = sort(unique(census$unidade)))
  mc$unidade[i] <- new_unidade
  if(old_unidade != new_unidade){
    message(i)
  }
}



# Match workers to census
# Define a function for getting similar dates of birth
dob_vary <- function(date){
  require(lubridate)
  date <- as.Date(date)
  c(date + years(1),
    date - years(1),
    date + years(10),
    date - years(10),
    date + months(1),
    date - months(1),
    date + days(1),
    date - days(1),
    date)
}

# Try to get the workers census identity
workers$census_name <- workers$census_name_match_score <- workers$perm_id <- workers$census_dob <- NA
for (i in 1:nrow(workers)){
  # Identify the worker
  this_worker_name <- workers$first_name_and_surname[i]
  this_dob <- workers$date_of_birth[i]
  # Define some potential dob variants
  dob_variants <- dob_vary(date = this_dob)
  # Get potential matches based on birthday
  same_birthday <- census %>%
    filter(dob %in% dob_variants) 
  # Also keep only those of the same sex
  if(!is.na(workers$sex[i])){
    same_birthday <- 
      same_birthday %>%
      filter(sex == workers$sex[i])
  }
  if(nrow(same_birthday) > 0){
    # Fuzzy match the names
    fuzzy_results <- cism::fuzzy_match(x = this_worker_name,
                                       y = same_birthday$name)
    if(!all(is.na(fuzzy_results[1,]))){
      # Get best match
      best_match_indices <- 
        apply(fuzzy_results, 1, function(x){
          which.min(x)
        })
      best_match_scores <- 
        apply(fuzzy_results, 1, function(x){
          min(x, na.rm = TRUE)
        })
      # Subset to just keep best matches
      sub_same_birthday <- same_birthday[best_match_indices,]
      # If ties, break with birthday
      if(nrow(sub_same_birthday) > 1){
        sub_match_index <- which.min(abs(as.numeric(this_dob - sub_same_birthday$dob)))[1]
        sub_same_birthday <- sub_same_birthday[sub_match_index,]
        best_match_scores <- best_match_scores[sub_match_index]
      }
      # Extract the info
      workers$census_name[i] <- sub_same_birthday$name
      workers$census_name_match_score[i] <- best_match_scores
      workers$perm_id[i] <- sub_same_birthday$perm_id
      message(i, ': Matched ', this_worker_name, ' with ', sub_same_birthday$name, '(score: ',
              best_match_scores, ').')
    }
  }
}

# Get the fixed coordinates (emailed by aura)
coordenadas <- read_csv('Coordenadas.csv')
# convert to lat lon
ll <- cism::ll_from_utm(coordenadas$LongUTM, coordenadas$LatUTM)
# Join
coordenadas <- bind_cols(coordenadas, ll)
# Get an unidade
coordenadas$unidade <- gsub('-', '', coordenadas$Family_id)
# Clean up
coordenadas <-
  coordenadas %>%
  rename(longitude_aura = x,
         latitude_aura = y) %>%
  dplyr::select(unidade,
                longitude_aura,
                latitude_aura)
  
# Join to census
census <-
  left_join(census,
            coordenadas)

# Join to mc
mc <-
  left_join(mc,
            coordenadas)

# Save again for use in package
# Save for use in package
devtools::use_data(ab,
                   overwrite = TRUE)
devtools::use_data(clinic,
                   overwrite = TRUE)
devtools::use_data(clinic_agg,
                   overwrite = TRUE)
devtools::use_data(workers,
                   overwrite = TRUE)
devtools::use_data(ab_panel,
                   overwrite = TRUE)
devtools::use_data(bairros,
                   overwrite = TRUE)
devtools::use_data(census,
                   overwrite = TRUE)
devtools::use_data(mc,
                   overwrite = TRUE)

##################################################
# Save a backup
save.image(paste0('../backups/', Sys.Date(), '.RData'))

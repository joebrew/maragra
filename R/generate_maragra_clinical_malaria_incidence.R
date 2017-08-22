#' Generate Maragra clinical malaria incidence report
#' 
#' Generate an html report of Maragra clinical malaria incidence, based
#' on aggregate numbers manually collected by Joe in December 2016 at the 
#' Maragra clinic and laboratory
#' @param date A date to be printed on the report. If \code{NULL} (the 
#' default), the current date will be used
#' @param output_dir The directory to which the file should be written. If 
#' \code{NULL} (the default), the current working directory will be used.
#' @param output_file The name of the file to be written. 
#' @return An html will be written
#' @importFrom rmarkdown render
#' @export

generate_maragra_clinical_malaria_incidence <- function(date = NULL,
                                                     output_dir = NULL,
                                                     output_file = 'malaria_clinical_malaria_incidence.html'){
  
  # If no output directory, make current wd
  if(is.null(output_dir)){
    output_dir <- getwd()
  }
  
  # If not date, use today's
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  # Combine parameters into a list, so as to pass to Rmd
  parameters <- list(date = date)
  
  # Find location the rmd to knit
  file_to_knit <- 
    system.file('rmd/maragra_clinical_malaria_incidence.Rmd', 
                package='maragra')
  
  # Knit file
  rmarkdown::render(file_to_knit,
                    output_dir = output_dir,
                    output_file = output_file,
                    params = parameters)
}

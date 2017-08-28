#' Generate preliminary analysis report
#' 
#' Generate an html report of the preliminary analysis report,
#' originally prepared in August / September 2017
#' @param date A date to be printed on the report. If \code{NULL} (the 
#' default), the current date will be used
#' @param output_dir The directory to which the file should be written. If 
#' \code{NULL} (the default), the current working directory will be used.
#' @param output_file The name of the file to be written. 
#' @return An html will be written
#' @importFrom rmarkdown render
#' @export

generate_preliminary_analysis <- function(date = NULL,
                                                     output_dir = NULL,
                                                     output_file = 'generate_preliminary_analysis.html'){
  
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
    system.file('rmd/maragra_preliminary_analysis.Rmd', 
                package='maragra')
  
  # Knit file
  rmarkdown::render(file_to_knit,
                    output_dir = output_dir,
                    output_file = output_file,
                    params = parameters)
}

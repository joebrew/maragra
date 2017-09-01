#' Run all analyses
#' 
#' Run all analyses and produce outputs in the \code{outputs_dir} folder
#' @param date A date to be printed on the report. If \code{NULL} (the 
#' default), the current date will be used
#' @param output_dir The directory to which the analyses should be written. If 
#' \code{NULL} (the default), the current working directory will be used.
#' @return html and pdf files will be written
#' @importFrom rmarkdown render
#' @export

run_all_analyses <- function(date = NULL,
                             output_dir = NULL){
  
  # If no output directory, make current wd
  if(is.null(output_dir)){
    output_dir <- getwd()
  }
  
  # If not date, use today's
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  # Clinical malaria incidence report
  message(paste0('Generating the clinical malaria incidence report in ',
                 output_dir))
  generate_maragra_clinical_malaria_incidence(date = date,
                                              output_dir = output_dir)
  
  # Clinical preliminary analysis report
  message(paste0('Generating the preliminary analysis report in ',
                 output_dir))
  generate_preliminary_analysis(date = date,
                                output_dir = output_dir)
  
  # Clinical preliminary analysis report
  message(paste0('Generating the paper in ',
                 output_dir))
  generate_paper(date = date,
                 output_dir = output_dir)
  
}
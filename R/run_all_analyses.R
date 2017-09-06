#' Run all analyses
#' 
#' Run all analyses and produce outputs in the \code{outputs_dir} folder
#' @param only A vector of .Rmd file names (excluding path) to be knitted
#' @param except A vector or .Rmd file names (excluding path) to be excluded
#' @param date A date to be printed on the report. If \code{NULL} (the 
#' default), the current date will be used
#' @param output_dir The directory to which the analyses should be written. If 
#' \code{NULL} (the default), the current working directory will be used.
#' @return html and pdf files will be written
#' @importFrom rmarkdown render
#' @export

run_all_analyses <- function(only = NULL,
                             except = NULL,
                             date = NULL,
                             output_dir = NULL){
  
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
  
  # Identify all files for knitting
  rmds <- dir(system.file('rmd/', package = 'maragra'))
  rmds <- rmds[grepl('.Rmd', rmds)]
  
  # Include / exclude if necessary
  if(!is.null(only)){
    rmds <- rmds[rmds %in% only]
  }
  if(!is.null(except)){
    rmds <- rmds[!rmds %in% except]
  }
  
  # Make sure still more than one file
  if(length(rmds) < 1){
    stop("No files to knit. Consider setting the 'only' and 'except' to NULL (the default).")
  }
  
  # Loop through each file and knit
  for(i in 1:length(rmds)){
    this_file <- rmds[i]
    file_to_knit <- system.file(paste0('rmd/',
                                       this_file), 
                                package='maragra')
    message(paste0('Generating ',
                   this_file,
                   ' in ',
                   output_dir))
    
    # Knit file
    rmarkdown::render(file_to_knit,
                      output_dir = output_dir,
                      output_file = this_file,
                      params = parameters)
  }
  # # Clinical malaria incidence report
  # message(paste0('Generating the clinical malaria incidence report in ',
  #                output_dir))
  # generate_maragra_clinical_malaria_incidence(date = date,
  #                                             output_dir = output_dir)
  # 
  # # Clinical preliminary analysis report
  # message(paste0('Generating the preliminary analysis report in ',
  #                output_dir))
  # generate_preliminary_analysis(date = date,
  #                               output_dir = output_dir)
  # 
  # # Clinical preliminary analysis report
  # message(paste0('Generating the paper in ',
  #                output_dir))
  # generate_paper(date = date,
  #                output_dir = output_dir)
  
}
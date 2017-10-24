#' Generate IHMT presentation
#'
#' Generate the presentation for the Oct 30-31 IHMT presentation
#' @param date A date to be printed on the report. If \code{NULL} (the
#' default), the current date will be used
#' @param output_dir The directory to which the file should be written. If
#' \code{NULL} (the default), the current working directory will be used.
#' @param output_file The name of the file to be written.
#' @return An html will be written
#' @importFrom rmarkdown render
#' @export

generate_ihmt_presentation <- function(output_dir = NULL,
                                       output_file = 'ihmt_presentation.pdf'){

  # If no output directory, make current wd
  if(is.null(output_dir)){
    output_dir <- getwd()
  }

  # Find location the rmd to knit
  file_to_knit <-
    system.file('rmd/ihmt_presentation_brew_2017.Rmd',
                package='maragra')

  # Knit file
  rmarkdown::render(file_to_knit,
                    output_dir = output_dir,
                    output_file = output_file)
}

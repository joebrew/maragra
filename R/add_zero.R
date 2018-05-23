#' Add zero
#'
#' Add a zero
#' @return character
#' @export
# Define function for adding zero
add_zero <- function (x, n) {
  x <- as.character(x)
  adders <- n - nchar(x)
  adders <- ifelse(adders < 0, 0, adders)
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      x[i] <- paste0(paste0(rep("0", adders[i]), collapse = ""),
                     x[i], collapse = "")
    }
  }
  return(x)
}
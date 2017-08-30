#' Correct the unidade
#' 
#' Try to find the right unidade of a non-conforming unidade
#' @param unidade An unidade
#' @param unidades A vector of all correct unidades
#' @return The correct unidade
#' @export

correct_unidade <- function(unidade,
                            unidades){
  out <- unidade
  if(nchar(unidade) == 8){
    # Chop off the first
    new_unidade <- substr(unidade, 1, 7)
    if(new_unidade %in% unidades){
      out <- new_unidade
    } else {
      # Chop off the last
      new_unidade <- substr(unidade, 2, 8)
      if(new_unidade %in% unidades){
        out <- new_unidade
      } 
    }
  }
  return(out)
}

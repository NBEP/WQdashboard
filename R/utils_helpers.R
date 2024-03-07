#' wrap_text
#'
#' @description Wrap long strings in list.
#'
#' @param old_list Input list.
#' @param str_len Length of string.
#' @param linebreak Linebreak symbol.
#'
#' @return Updated list.
wrap_text <- function(old_list, str_len, linebreak = NULL){
  new_list <- stringr::str_wrap(old_list, width = str_len)
  if(!is.null(linebreak)){
    new_list <- gsub("\n", linebreak, new_list)
  }
  return(new_list)
}

## NOT USING ABOVE ANYWHERE IN SCRIPT

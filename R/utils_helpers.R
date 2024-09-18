#' wrap_text
#' NOT USED IN PACKAGE - remove?
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

#' pretty_number
#'
#' @description Shorten number to two significant digits past the decimal point.
#'
#' @param x Input number.
#'
#' @return Shortened number.
#' @noRd
pretty_number <- function(x){
  y <- dplyr::if_else(
    x < 1,
    signif(x, 2),
    round(x, 2)
    )
  return(y)
}

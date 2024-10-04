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

#' Pretty Unit
#'
#' @description Convert parameter and unit to string in format "Parameter (Unit).
#'   If Unit is NA or None, format string as "Parameter".
#'
#' @param par Parameter.
#' @param par_unit Unit.
#'
#' @return String.
#' @noRd
pretty_unit <- function(par, par_unit){
  if (par_unit %in% c(NA, "None")) {
    return(par)
  }
  par <- paste0(par, " (", par_unit, ")")
  return(par)
}

#' Pretty List
#'
#' @description Convert list to string with appropriate placement of commas and
#'   use of "and".
#'
#' @param x List.
#'
#' @return String.
#' @noRd
pretty_list <- function(x){
  if (all(is.na(x))) {
    return(NA)
  }

  x <- x[!is.na(x)]

  if (length(x) > 2) {
    x <- paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
  } else if (length(x) == 2) {
    x <- paste(x[1], "and", x[2])
  }

  return(x)
}

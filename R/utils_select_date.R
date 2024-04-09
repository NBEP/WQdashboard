#' Sort months
#'
#' @description Orders months chronologically.
#'
#' @param month_list Unsorted list of months.
#'
#' @return Sorted list of months.
#'
#' @noRd
sort_months <- function(month_list){
  month_list <- unique(month_list)
  all_months <- c("January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December")

  all_months <- all_months[all_months %in% month_list]
  return(all_months)
}

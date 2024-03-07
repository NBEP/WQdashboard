#' Rename columns
#'
#' @description Renames columns in dataframe
#'
#' @param df Input dataframe.
#' @param old_field List of column names to be replaced.
#' @param new_field List of new column names.
#'
#' @return Dataframe with updated column names.
#'
#' @noRd
check_column_name <- function(df, old_field, new_field){
  if (length(old_field) != length(new_field)) {
    stop("old_field and new_field must be the same length")
  }

  # Rename columns
  names(new_field) <- old_field
  field_subs <- new_field[intersect(colnames(df), names(new_field))]

  if (length(field_subs) > 0){
    df <- df %>%
      dplyr::rename_with(~ field_subs, names(field_subs))
    message("\t", toString(length(field_subs)), " columns renamed")
  }

  return(df)
}

#' Check for missing columns
#'
#' @description Produces error message if any columns are missing.
#'
#' @param df Input dataframe.
#' @param field List of column names.
#'
#' @noRd
check_column_missing <- function(df, field) {
  # Modified code from MassWateR::checkMWRsites

  chk <- field %in% colnames(df)
  if(any(!chk)){
    tochk <- field[!chk]
    stop("\tThe following columns are missing: ",
         paste(tochk, collapse = ", "), call. = FALSE)
  }
}

#' Check for missing values
#'
#' @description Produces error message if any values are missing in column.
#'
#' @param df Input dataframe.
#' @param field List of column names.
#' @param is_stop Boolean. If TRUE, returns stop(). If FALSE, returns warning().
#'
#' @noRd
check_val_missing <- function(df, field, is_stop = TRUE) {
  # Modified code from MassWateR::checkMWRsites

  chk <- df[field]
  chk <- (!is.na(chk) & chk != "")
  if(any(!chk)){
    rws <- which(!chk)
    msg <- paste("\t", field, " missing in rows ", paste(rws, collapse = ", "))
    if (is_stop == TRUE){
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
}

#' Check for duplicate values
#'
#' @description Produces error message if any duplicate values in column. If
#'   more than one column listed, checks columns together as pair.
#'
#' @param df Input dataframe.
#' @param field Column name(s).
#' @param is_stop Boolean. If TRUE, returns stop(). If FALSE, returns warning().
#'
#' @noRd
check_val_duplicate <- function(df, field, is_stop=TRUE) {
  dup1 <- df %>%
    dplyr::select(all_of(field)) %>%
    duplicated()
  if(any(dup1)){
    dup2 <- df %>%
      dplyr::select(all_of(field)) %>%
      duplicated(fromLast = TRUE)
    dup_rws <- c(which(dup1), which(dup2)) %>%
      unique() %>%
      sort()
    msg <- paste0("\tDuplicate ", paste(field, collapse = ", "),
                  " in rows ", paste(dup_rws, collapse = ", "))
    if (is_stop == TRUE){
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
}

#' Drop column with less than 2 unique values
#'
#' @description Drops columns with less than 2 unique values.
#'
#' @param df Input dataframe.
#' @param field Column name.
#'
#' @return Updated dataframe.
#'
#' @noRd
check_val_count <- function(df, field) {

  if (!field %in% colnames(df)) {
    return(df)
  }

  chk <- unique(df[field])

  if (nrow(chk) < 2) {
    df <- dplyr::select(df, !all_of(field))
    message("\tRemoved column ", field, ": Less than 2 unique values")
  }

  return(df)
}

#' Check value numeric
#'
#' @description Produces error message if column value is not in numeric format
#'
#' @param df Dataframe.
#' @param field Column name.
#'
#' @noRd
check_val_numeric <- function(df, field) {
  # Modified code from MassWateR::checkMWRsites

  typ <- df[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ)))
  if(any(!chk)){
    rws <- which(!chk)
    stop("\tNon-numeric entries for ", field, " found in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
}

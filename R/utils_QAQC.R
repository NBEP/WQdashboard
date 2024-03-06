#' Duplicate column names
#'
#' @description Checks for duplicate column names
#'
#' @param df Input dataframe.
#'
#' @noRd
check_column_duplicate <- function(df){
  msg <- "\tChecking for duplicate column names..."

  field <- colnames(df)
  chk <- duplicated(field)

  if (any(chk)){
    tochk <- field[!chk] %>% unique()
    stop(msg, "\n\tThe following column names were used more than once: ",
         paste(tochk, collapse = ", "))
  }
  message(msg, "OK")
}


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

  msg <- "\tChecking column names..."

  # Rename columns
  names(new_field) <- old_field
  field_subs <- new_field[intersect(colnames(df), names(new_field))]

  if (length(field_subs) > 0){
    df <- df %>%
      rename_with(~ field_subs, names(field_subs))
    message(msg, toString(length(field_subs)), " columns renamed")
  } else {
    message(msg, "OK")
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

  msg <- "\tChecking all required columns are present..."
  chk <- field %in% colnames(df)
  if(any(!chk)){
    tochk <- field[!chk]
    stop(msg, "\n\tThe following columns are missing: ",
         paste(tochk, collapse = ", "), call. = FALSE)
  }
  message(paste(msg, "OK"))
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

  msg <- paste0("\tChecking for missing entries in ", field, "...")
  chk <- df[field]
  chk <- (!is.na(chk) & chk != "")
  if(any(!chk)){
    rws <- which(!chk)
    msg2 <- paste("\t", field, "missing in row", paste(rws, collapse = ", "))
    if (is_stop == TRUE){
      stop(msg, "\n", msg2, call. = FALSE)
    } else {
      warning(msg2, call. = FALSE)
    }
  } else {
    message(paste(msg, "OK"))
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
  msg <- paste0("\tChecking for duplicate values in ",
                paste(field, collapse = ", "), "...")
  dup1 <- df %>%
    select(all_of(field)) %>%
    duplicated()
  if(any(dup1)){
    dup2 <- df %>%
      select(all_of(field)) %>%
      duplicated(fromLast = TRUE)
    dup_rws <- c(which(dup1), which(dup2)) %>%
      unique() %>%
      sort()
    msg2 <- paste0("\tDuplicate ", paste(field, collapse = ", "),
                  " in row ", paste(dup_rws, collapse = ", "))
    if (is_stop == TRUE){
      stop(msg, "\n", msg2, call. = FALSE)
    } else {
      warning(msg2, call. = FALSE)
    }
  } else {
    message(paste(msg, "OK"))
  }
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

  msg <- paste("\tChecking for non-numeric values in", field, "...")
  typ <- df[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ)))
  if(any(!chk)){
    rws <- which(!chk)
    stop(msg, "\n\tNon-numeric entries for ", field, " found in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
  message(paste(msg, "OK"))
}

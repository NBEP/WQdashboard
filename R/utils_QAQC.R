#' Detect data format
#'
#' @description Uses column names to detect data format. (eg WQX, WQdashboard,
#'   Watershed Watch, etc)
#'
#' @param df Input dataframe.
#' @param df_colnames Dataframe with column name substitutions. Acceptable
#'  entries are `colnames_sites`, `colnames_results`.
#'
#' @returns Name of data format as string.
#'
#' @noRd
detect_column_format <- function(df, df_colnames){
  msg <- paste("\tDetecting format...")
  ok_colnames <- c(colnames_sites, colnames_results)
  if(!all(df_colnames %in% ok_colnames)){
    stop("Invalid df_colnames. Acceptable inputs: colnames_sites,
         colnames_results")
  }

  # Iterate through formats
  for (x in colnames(df_colnames)) {
    df_format <- df_colnames %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::filter_at(x, dplyr::all_vars(!is.na(.) & . != ""))
    # Check if df matches selected format
    chk <- unlist(df_format) %in% colnames(df)
    if(all(chk)){
      message(msg, x)
      return(x)
    }
  }
  stop(msg, "Invalid format. Acceptable formats include: ",
       paste(colnames(df_colnames), collapse = ", "), call. = FALSE)
}

#' Update column names
#'
#' @description Updates column names to WQdashboard format
#'
#' @param df Input dataframe.
#' @param df_colnames Dataframe with column name substitutions. Acceptable
#'  entries are `colnames_sites`, `colnames_results`.
#'
#' @returns Updated dataframe.
#'
#' @noRd
update_column_names <- function(df, df_colnames){

  current_format <- detect_column_format(df, df_colnames)

  if (current_format %in% c("WQdashboard", "WQdashboard_short")) {
    return(df)
  }

  df_format <- df_colnames %>%
    dplyr::select("WQdashboard", dplyr::all_of(current_format)) %>%
    dplyr::filter_at(current_format, dplyr::all_vars(!is.na(.) & . != ""))

  old_field <- unlist(df_format[current_format])
  new_field <- unlist(df_format$WQdashboard)
  names(new_field) <- old_field
  field_subs <- new_field[intersect(colnames(df), names(new_field))]

  df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))

  message("\t", toString(length(field_subs)), " columns renamed")
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

#' Ignore dq rows
#'
#' @description Updates check to ignore rows where `Qualifier` listed in
#'   `qaqc_fail`. Mini helper function for other QAQC checks.
#'
#' @param df Dataframe.
#' @param chk Existing QAQC check.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_dq_rows <- function(df, chk) {
  if("Qualifier" %in% colnames(df)) {
    chk <- (chk | df$Qualifier %in% qaqc_fail)
  }
  return(chk)
}

#' Ignore qc rows
#'
#' @description Updates check to ignore rows where `Activity_Type` is
#'   `Quality Control`.
#'
#' @param df Dataframe.
#' @param chk Existing QAQC check.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_qc_rows <- function(df, chk) {
  if("Activity_Type" %in% colnames(df)) {
    chk <- (chk | stringr::str_detect(df$Activity_Type, "Quality Control"))
  }
  return(chk)
}

#' Check for missing values
#'
#' @description Produces error message if any values are missing in column.
#'
#' @param df Input dataframe.
#' @param field List of column names.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default TRUE.
#' @param is_stop Boolean. If TRUE, returns stop(). If FALSE, returns warning().
#'   Default TRUE.
#'
#' @noRd
check_val_missing <- function(df, field, ignore_dq = TRUE, ignore_qc = TRUE,
                              is_stop = TRUE) {
  # Modified code from MassWateR::checkMWRsites

  df_field <- df[field]
  chk <- !is.na(df_field)
  if(ignore_dq) { chk <- skip_dq_rows(df, chk) }
  if(ignore_qc) { chk <- skip_qc_rows(df, chk) }

  if(any(!chk)){
    rws <- which(!chk)
    msg <- paste("\t", field, "missing in rows", paste(rws, collapse = ", "))
    if (length(rws) > 20){
      msg <- paste("\t", field, "missing in", toString(length(rws)), "rows.")
    }
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
    dplyr::select(dplyr::all_of(field)) %>%
    duplicated()
  if(any(dup1)){
    dup2 <- df %>%
      dplyr::select(dplyr::all_of(field)) %>%
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
    df <- dplyr::select(df, !dplyr::all_of(field))
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
#' @param exceptions List of acceptable string values. Default NULL.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default FALSE.
#'
#' @noRd
check_val_numeric <- function(df, field, exceptions = NULL, ignore_dq = TRUE,
                              ignore_qc = FALSE) {
  # Modified code from MassWateR::checkMWRsites

  typ <- df[field]
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ)))
  if(ignore_dq) { chk <- skip_dq_rows(df, chk) }
  if(ignore_qc) { chk <- skip_qc_rows(df, chk) }
  if(!is.null(exceptions)){
    chk <- (chk | sapply(typ, function(x) x %in% exceptions))
  }

  if(any(!chk)){
    rws <- which(!chk)
    stop("\tNon-numeric entries for ", field, " found in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
}

#' Format date column
#'
#' @description Checks if "Date" column is date format, converts to date if not.
#'
#' @param df Dataframe.
#' @param date_format Date format.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#'
#' @noRd
format_date_col <- function(df, date_format, ignore_dq = TRUE) {
  chk <- mapply(lubridate::is.Date, df$Date)
  if (ignore_dq) {
    df_temp <- dplyr::filter(df, !Qualifier %in% qaqc_fail)
    chk <- mapply(lubridate::is.Date, df_temp$Date)
  }

  if(all(chk)){
    return(df)
  } else if (is.null(date_format)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c("a", "A", "b", "B", "d", "H", "I", "j", "q", "m", "M", "p", "S",
    "OS", "U", "w", "W", "y", "Y", "z", "Om", "Op", "r", "R", "T")
  chk <- gsub("[^a-zA-Z]", "", date_format)
  chk <- gsub(paste(unlist(date_var), collapse = "|"), "", chk)

  if(length(chk) > 0 & chk != "") {
    stop("date_format contains invalid variables: ",
         paste(chk, collapse = ", "), call. = FALSE)
  }

  df <- dplyr::mutate(df,
    Date = as.Date(lubridate::parse_date_time(as.character(Date), date_format,
                                   quiet = TRUE)))

  chk <- !is.na(df$Date)
  chk2 <- chk
  if (ignore_dq) { chk <- skip_dq_rows(df, chk) }

  if(all(!chk) | all(!chk2)) {
    stop('Date column does not match format "', date_format, '"',
         call. = FALSE)
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop("Date is improperly formatted in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }
  return(df)
}

#' Rename parameters
#'
#' @description Standardizes parameter names to match WQX format. Requires
#'   `param_names` be up to date.
#'
#' @param param Old parameter name.
#'
#' @return New parameter name.
#'
#' @noRd
rename_param <- function(param) {
  if (param %in% names(param_names)) {
    param <- param_names[[param]]
  }
  return(param)
}

#' Rename units
#'
#' @description Standardizes unit names to match WQX format. Requires
#'   `unit_nmaes` be up to date.
#'
#' @param unit Old unit name.
#'
#' @return New unit name.
#'
#' @noRd
rename_unit <- function(unit) {
  if (unit %in% names(unit_names)) {
    unit <- unit_names[[unit]]
  }
  return(unit)
}

#' Convert units
#'
#' @description Uses values in `unit_conversion` to convert units.
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#' @param is_stop If TRUE, provides error message for invalid results.
#'    Default TRUE.
#'
#' @return Updated value.
#'
#' @noRd
convert_unit <- function(x, old_unit, new_unit, is_stop = TRUE) {
  # Check if null
  if (is.na(old_unit) | is.na(new_unit)) {
    return(x)
  }
  # Standardize names
  old_unit <- rename_unit(old_unit)
  new_unit <- rename_unit(new_unit)
  # Check if identical
  if (old_unit == new_unit) {
    return(x)
  }
  # Run conversion
  old_new <- data.frame(Unit=old_unit, Unit_2=new_unit)
  old_new <- merge(unit_conversion, old_new)
  new_old <- data.frame(Unit=new_unit, Unit_2=old_unit)
  new_old <- merge(unit_conversion, new_old)
  if(nrow(old_new) > 0){
    a <- old_new$Conversion_Multiply[1]
    b <- old_new$Conversion_Add[1]
    x <- a*x + b
  } else if (nrow(new_old) > 0 ){
    a <- new_old$Conversion_Multiply[1]
    b <- new_old$Conversion_Add[1]
    x <- (x-b)/a
  } else if (is_stop) {
    stop("Unable to convert ", old_unit, " to ", new_unit)
  }
  return(x)
}

#' Check units
#'
#' @description Checks if more than one `Result_Unit` per `Parameter`. Creates
#'   error message listing problem `Parameter` values.
#'
#' @param df Dataframe
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default TRUE.
#'
#' @noRd
check_units <- function(df, ignore_dq = TRUE, ignore_qc = TRUE) {
  if (ignore_dq & "Qualifier" %in% colnames(df)) {
    df <- dplyr::filter(df, !df$Qualifier %in% qaqc_fail)
  }
  if (ignore_qc & "Activity_Type" %in% colnames(df)) {
    df <- dplyr::filter(df,
      !stringr::str_detect(Activity_Type, "Quality Control"))
  }

  df <- df %>%
    dplyr::select(Parameter, Result_Unit) %>%
    unique()

  chk <- !duplicated(df$Parameter)

  if (any(!chk)) {
    dup_rws <- which(!chk)
    dup_param <- df$Parameter[dup_rws] %>%
      unique() %>%
      sort()
    stop("Only one unit allowed per parameter. Multiple units listed for:\n\t-",
         paste(dup_param, collapse = "\n\t-"), call. = FALSE)
  }
}

#' Converts depth to m
#'
#' @description Converts columns Depth, Depth_Unit to meters.
#'
#' @param df Dataframe
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail`. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control". Default TRUE.
#' @param ignore_depth_param Boolean. If TRUE, ignores rows where `Parameter` is
#'   a depth reading.
#'
#' @noRd
depth_to_m <- function(df) {
  if (!"Depth_Unit" %in% colnames(df)) {
    stop("The following column is missing: Depth_Unit", call. = FALSE)
  }
  check_val_numeric(df, "Depth", exceptions = NA)

  # Exempt rows
  exempt <- stringr::str_detect(df$Parameter, "Depth")
  exempt <- skip_dq_rows(df, exempt)
  exempt <- skip_qc_rows(df, exempt)

  if ("Depth_Category" %in% colnames(df)) {
    ok_cat <- c("Shallow", "Deep")
    exempt <- (exempt | df$Depth_Category %in% c(ok_cat))
    chk <- (exempt | df$Depth_Category %in% NA)
    if (any(!chk)) {
      stop("Invalid Depth_Category. Acceptable values: ",
           paste(c(ok_cat,NA), collapse = ", "), ". Check rows ",
           paste(rws, collapse = ", "), call. = FALSE)
    }
  }

  chk <- (!is.na(df$Depth) | exempt)
  if(any(!chk)) {
    rws <- which(!chk)
    warning("\tDepth is missing in rows ", paste(rws, collapse = ", "),
            call. = FALSE)
  }
  chk <- (!is.na(df$Depth_Unit) | exempt)
  if(any(!chk)) {
    rws <- which(!chk)
    warning("\tDepth_Unit is missing in rows ", paste(rws, collapse = ", "),
            call. = FALSE)
  }
  chk <- (df$Depth_Unit %in% c(NA, "m") | exempt)
  if (all(chk)) {
    return(df)
  }

  ok_units <- c("in", "ft", "cm", "m")
  chk <- df$Depth_Unit %in% c(ok_units, NA)
  chk <- (chk | exempt)

  if (any(!chk)) {
    rws <- which(!chk)
    stop("Invalid Depth_Unit. Acceptable values: ",
         paste(ok_units, collapse = ", "), ". Check rows ",
         paste(rws, collapse = ", "), call. = FALSE)
  }

  df_temp <- df %>%
    dplyr::select(Depth, Depth_Unit) %>%
    dplyr::filter(!is.na(Depth)) %>%
    dplyr::filter(Depth_Unit %in% ok_units[ok_units != "m"]) %>%
    unique() %>%
    dplyr::mutate(temp_depth =
      mapply(function(x, y) convert_unit(x, y, "m"), Depth, Depth_Unit))

  df <- dplyr::left_join(df, df_temp,
      by = dplyr::join_by(Depth, Depth_Unit)) %>%
    dplyr::mutate(Depth = dplyr::if_else(
      is.na(temp_depth),
      Depth,
      temp_depth)) %>%
    dplyr::mutate(Depth_Unit = dplyr::if_else(
      is.na(temp_depth),
      Depth_Unit,
      "m")) %>%
    dplyr::select(!temp_depth)
  message("\tConverted depth to meters")
  return(df)
}

#' Assign depth category
#'
#' @description Assigns depth category. Run `depth_to_m` first.
#'
#' @param df Input dataframe.
#' @param overwrite_cat Boolean. If TRUE, replaces existing depth_category
#'   scores with calculated scores. Default FALSE.
#' @param sites Site dataframe. Only present for testing purposes.
#'
#' @noRd
assign_depth_category <- function(df, overwrite_cat = FALSE,
                                  sites = df_sites) {
  col_keep <- c(colnames(df), "Depth_Category")

  if (!"Depth_Category" %in% colnames(df)) {
    message("Added column Depth_Category")
    df <- dplyr::mutate(df, Depth_Category=NA)
  }

  if (overwrite_cat) {
    chk <- (is.na(df$Depth_Category) | is.na(df$Depth) | df$Depth_Unit != "m")
    if (any(!chk)) {
      rws <- which(!chk)
      df$Depth_Category[rws] <- NA
      warning("Recalculating depth category for rows ",
        paste(rws, collapse = ", "), call. = FALSE)
    }
  }

  if (!"Max_Depth_Shallow" %in% colnames(sites)) {
    df <- dplyr::mutate(df, Max_Depth_Shallow = NA)
  }

  df <- dplyr::left_join(df, sites, by="Site_ID", keep = FALSE) %>%
    dplyr::mutate(df, Max_Depth_Shallow = dplyr::if_else(
      is.na(Max_Depth_Shallow), 1, Max_Depth_Shallow)) %>%
    dplyr::mutate(Depth_Category = dplyr::case_when(
      !is.na(Depth_Category) ~ Depth_Category,
      is.na(Depth) | Depth_Unit != "m" ~ Depth_Category,
      Depth > Max_Depth_Shallow ~ "Deep",
      TRUE ~ "Shallow")) %>%
    dplyr::select(dplyr::all_of(col_keep))

  return(df)
}

#' List sites
#'
#' @description Lists unique sites by Site_ID.
#'
#' @param df Dataframe.
#' @param ignore_dq Boolean. If TRUE, ignores rows where `Qualifier` listed in
#'   `qaqc_fail` and Site_ID is NA. Default TRUE.
#' @param ignore_qc Boolean. If TRUE, ignores rows where `Activity_Type` starts
#'   with "Quality Control" and Site_ID is NA. Default TRUE.
#'
#' @return List of unique Site_ID values.
#'
#' @noRd
list_sites <- function(df, ignore_dq = TRUE, ignore_qc = TRUE) {
  if (ignore_dq & "Qualifier" %in% colnames(df)) {
    df <- dplyr::filter(df,
      !(Qualifier %in% qaqc_fail & is.na(Site_ID)))
  }
  if (ignore_qc & "Activity_Type" %in% colnames(df)) {
    df <- dplyr::filter(df,
      !(stringr::str_detect(Activity_Type, "Quality Control")
        & is.na(Site_ID)))
  }
  unique_sites <- unique(df$Site_ID)

  return(unique_sites)
}

#' Find variable names
#'
#' @description Extracts list of old and new variable names from a dataframe.
#'
#' @param df Dataframe with variable columns. Each format is assigned a column
#'   and equivalent variables are paired across each row.
#' @param in_format Input format.
#' @param out_format Output format.
#'
#' @returns List of paired old and new variable names, list of all new variable
#'   names.
#'
#' @noRd
find_var_names <- function(df, in_format, out_format){

  # Check if df is dataframe
  chk <- inherits(df, "data.frame")
  if(!chk) {
    stop("df must be type dataframe")
  }

  # Check if dataframe includes in_format, out_format
  chk <- in_format %in% colnames(df)
  chk2 <- out_format %in% colnames(df)
  if (!chk & !chk2) {
    stop("Invalid in_format and out_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  } else if (!chk) {
    stop("Invalid in_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  } else if (!chk2) {
    stop("Invalid out_format. Acceptable formats: ",
         paste(colnames(df), collapse=", "))
  }

  # List new column names
  keep_var <- df[[out_format]]
  keep_var <- keep_var[!is.na(keep_var)]

  # Create matched list of old names, new names
  df <- df %>%
    dplyr::select(dplyr::all_of(c(in_format, out_format))) %>%
    dplyr::filter_at(out_format, dplyr::all_vars(!is.na(.)  & . != "")) %>%
    dplyr::filter_at(in_format, dplyr::all_vars(!is.na(.) & . != "")) %>%
    # If out_format includes multiple vars, only keep first value
    dplyr::mutate(
      {{out_format}} := dplyr::if_else(
        grepl("|", .data[[out_format]], fixed=TRUE),
        stringr::str_split_i(.data[[out_format]], "\\|", 1),
        .data[[out_format]]
      ))
  # If in_format includes multiple vars, split to multiple rows
  df <- tidyr::separate_longer_delim(df, {{in_format}}, "|") %>%
    dplyr::filter(.data[[in_format]] != .data[[out_format]])

  if (nrow(df) > 0) {
    old_names <- unlist(df[in_format])
    names(old_names) <- NULL
    new_names <- unlist(df[out_format])
    names(new_names) <- NULL
  } else {
    old_names <- NA
    new_names <- NA
  }

  return(
    list(
      old_names = old_names,
      new_names = new_names,
      keep_var = keep_var
    )
  )
}

#' Rename columns
#'
#' @description Renames columns to match output format.
#'
#' @param df Input dataframe.
#' @param old_colnames Old column names.
#' @param new_colnames New column names.
#'
#' @returns Updated dataframe.
#'
#' @noRd
rename_col <- function(df, old_colnames, new_colnames) {
  # Check inputs
  chk <- all(is.na(old_colnames))
  chk2 <- all(is.na(new_colnames))
  if (chk & chk2) {
    return(df)
  } else if (chk | chk2) {
    stop("old_colnames and new_colnames are different lengths")
  } else if (all(old_colnames == new_colnames)) {
    return(df)
  } else if (length(old_colnames) != length(new_colnames)) {
    stop("old_colnames and new_colnames are different lengths")
  }

  # Rename columns
  names(new_colnames) <- old_colnames
  new_colnames <- new_colnames[!is.na(new_colnames)]
  new_colnames <- new_colnames[!is.na(names(new_colnames))]
  field_subs <- new_colnames[intersect(colnames(df), names(new_colnames))]

  if (length(field_subs) > 0) {
    df <- dplyr::rename_with(df, ~ field_subs, names(field_subs))
  }

  return(df)
}

#' Rename variable
#'
#' @description Updates variable name. Helper function for `rename_all_var`.
#'
#' @param in_var Variable to update.
#' @param old_varname List of old variable names.
#' @param new_varname List of new variable names.
#'
#' @return New variable name.
#'
#' @noRd
rename_var <- function(in_var, old_varname, new_varname) {
  names(new_varname) <- old_varname

  if (in_var %in% old_varname) {
    in_var <- new_varname[[in_var]]
  }

  return(in_var)
}

#' Rename all variables in column
#'
#' @description Updates variable names in column.
#'
#' @param df Input dataframe.
#' @param col_name Column name.
#' @param old_varname List of old variable names.
#' @param new_varname List of new variable names.
#'
#' @return Updated dataframe.
#'
#' @noRd
rename_all_var <- function(df, col_name, old_varname, new_varname) {

  # Check inputs
  chk <- col_name %in% colnames(df)
  if (!chk) {
    stop("col_name not in dataframe")
  }
  chk <- all(is.na(old_varname))
  chk2 <- all(is.na(new_varname))
  if (chk & chk2) {
    return(df)
  } else if (chk | chk2) {
    stop("old_varname and new_varname are different lengths")
  } else if (all(old_varname == new_varname)) {
    return(df)
  } else if (length(old_varname) != length(new_varname)) {
    stop("old_varname and new_varname are different lengths")
  }

  # Update variable names
  df <- df %>%
    dplyr::mutate(
      {{col_name}} := sapply(
        .data[[col_name]],
        function(x) rename_var(x, old_varname, new_varname))) %>%
    dplyr::mutate({{col_name}} := unname(.data[[col_name]])) # remove names

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

#' Skip Rows
#'
#' @description Lists rows where data is marked as suspect or a QAQC check.
#'
#' @param df Dataframe.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_rows <- function(df) {
  if ("Qualifier" %in% colnames(df)) {
    chk <- df$Qualifier %in% qaqc_flag$suspect
  } else {
    chk <- rep(FALSE, nrow(df))
  }

  if("Activity_Type" %in% colnames(df)) {
    chk2 <- !is.na(df$Activity_Type) &
      stringr::str_detect(df$Activity_Type, "Quality Control")
    chk <- chk | chk2
  }

  return(chk)
}

# Set Nondetect Values
#'
#' @description Sets values for non-detect data. If detection limits are
#'  provided, non-detect data is set to half the detection limit. If detection
#'  limits are not provided, non-detect data is set to zero.
#'
#' @param df Dataframe.
#'
#' @return Updated dataframe
#'
#' @noRd
set_nondetect_values <- function(df) {
  chk <- df$Result %in% c("BDL")
  if("Qualifier" %in% colnames(df)) {
    chk <- chk | df$Qualifier %in% qaqc_flag$nondetect
  }
  chk <- chk & !skip_rows(df)

  if (all(!chk)) { return(df) }

  df_d <- df[!chk,]
  df_nd <- df[chk,] %>%
    dplyr::mutate(
      Qualifier = dplyr::if_else(
        is.na(Qualifier),
        "DL",
        Qualifier
      )
    )

  if (all(c("Detection_Limit", "Detection_Limit_Unit") %in% colnames(df))) {
    check_val_numeric(df, "Detection_Limit")
    check_val_missing(df, "Detection_Limit_Unit")
    df_nd <- df_nd %>%
      dplyr::mutate(
        Result = dplyr::case_when(
          is.na(Detection_Limit) ~ 0,
          Detection_Limit <= 0 ~ Detection_Limit,
          TRUE ~ Detection_Limit/2
        )
      ) %>%
      dplyr::mutate(Result_Unit = Detection_Limit_Unit)
  } else {
    df_nd <- df_nd %>%
      dplyr::mutate(Result = 0) %>%
      dplyr::select(!Result_Unit)

    df_temp <- df_d %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarise("Result_Unit" = dplyr::last(Result_Unit))

    df_nd <- dplyr::left_join(df_nd, df_temp, by = dplyr::join_by(Parameter))
  }

  df <- rbind(df_d, df_nd)

  return(df)
}

#' Check for missing values
#'
#' @description Produces error message if any values are missing in column.
#'
#' @param df Input dataframe.
#' @param field Column name.
#' @param is_stop Boolean. If TRUE, returns stop(). If FALSE, returns warning().
#'   Default TRUE.
#'
#' @noRd
check_val_missing <- function(df, field, is_stop = TRUE) {
  df_field <- df[field]
  chk <- !is.na(df_field) | skip_rows(df)

  if(any(!chk)){
    rws <- which(!chk)
    if (length(rws) > 20){
      msg <- paste0("\t", field, " missing in ", toString(length(rws)), " rows")
    } else {
      msg <- paste0("\t", field, " missing in rows ", paste(rws, collapse = ", "))
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
#' @param field List of column names.
#'
#' @return Updated dataframe.
#'
#' @noRd
check_val_count <- function(df, field) {

  field <- intersect(field, colnames(df))

  if (length(field) == 0) { return(df) }

  field_len <- lapply(df, dplyr::n_distinct)
  drop_field <- names(field_len[field_len < 2])
  drop_field <- intersect(field, drop_field)

  if (length(drop_field) == 0) { return(df) }

  df <- dplyr::select(df, !dplyr::all_of(drop_field))
  message("\tRemoved column ", paste(drop_field, collapse = ", "),
          ": Less than 2 unique values")

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
  chk <- !is.na(suppressWarnings(mapply(as.numeric, typ))) | is.na(typ)

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
#' @param date_col Name of date column.
#' @param date_format Date format.
#'
#' @noRd
format_date <- function(df, date_col, date_format="m/d/Y") {

  df_temp <- dplyr::filter(df, !is.na(.data[[date_col]]))
  chk <- mapply(lubridate::is.Date, df_temp[[date_col]])

  if(all(chk)){
    return(df)
  } else if (is.null(date_format)) {
    stop("Date format is missing", call. = FALSE)
  }

  date_var <- c("OS", "Om", "Op", "a", "A", "b", "B", "d", "H", "I", "j", "q",
                "m", "M", "p", "S", "U", "w", "W", "y", "Y", "z", "r", "R", "T")
  chk_var <- gsub("[^a-zA-Z]", "", date_format)
  chk_var <- gsub(paste(unlist(date_var), collapse = "|"), "", chk_var)

  if(chk_var != "") {
    stop("date_format contains invalid variables: ", chk_var, call. = FALSE)
  }

  chk <- is.na(df[[date_col]])

  df <- df %>%
    dplyr::mutate(
      {{date_col}} := as.Date(
        lubridate::parse_date_time(
          as.character(.data[[date_col]]),
          date_format,
          quiet = TRUE)))

  chk2 <- !is.na(df[[date_col]])
  chk <- chk | chk2

  if(all(!chk2)) {
    stop('Date does not match format "', date_format, '"',
         call. = FALSE)
  } else if (any(!chk)) {
    rws <- which(!chk)
    stop("Date is improperly formatted in rows: ",
         paste(rws, collapse = ", "), call. = FALSE)
  }

  return(df)
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
  if (old_unit == new_unit) { return(x) }

  # Standardize names
  var_names <- find_var_names(varnames_units, "WQX", "measurements")

  old_unit <- rename_var(old_unit, var_names$old_names, var_names$new_names)
  new_unit <- rename_var(new_unit, var_names$old_names, var_names$new_names)

  # Run conversion
  chk <- grepl("/", c(old_unit, new_unit))
  if (any(chk)) {
    y <- try(
      measurements::conv_multiunit(x, old_unit, new_unit),
      silent = TRUE
    )
  } else {
    y <- try(
      measurements::conv_unit(x, old_unit, new_unit),
      silent = TRUE
    )
  }

  if (class(y) != "try-error") {
    return(y)
  } else if (is_stop) {
    stop("Unable to convert ", old_unit, " to ", new_unit, call. = FALSE)
  } else {
    return(NA)
  }
}

#' Standardize Units
#'
#' @description Checks if more than one `Result_Unit` per `Parameter`. If more
#'   than one unit listed, standardizes parameter units according to most recent
#'   result.
#'
#' @param df Dataframe
#'
#' @noRd
standardize_units <- function(df) {
  chk <- skip_rows(df)
  df2 <- df[which(!chk),]

  df_temp <- df2 %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarise("temp_unit" = dplyr::last(Result_Unit))

  df2 <- dplyr::left_join(df2, df_temp, by = dplyr::join_by(Parameter)) %>%
    dplyr::filter(Result_Unit != temp_unit)

  if (nrow(df2) == 0) { return(df) }

  df2 <- df2 %>%
    dplyr::select(Parameter, Result, Result_Unit, temp_unit) %>%
    unique() %>%
    dplyr::mutate(
      temp_result = mapply(
        function(x, y, z) convert_unit(x, y, z),
        Result, Result_Unit, temp_unit
      )
    )

  chk <- is.na(df2$temp_result)
  if (any(chk)) {
    rws <- which(chk)
    df_error <- df2[rws,]
    stop("Only one unit allowed per parameter. Unable to standardize units for:\n\t-",
         paste(unique(df_error$Parameter), collapse = "\n\t-"), call. = FALSE)
  }

  df <- dplyr::left_join(
      df,
      df2,
      by = dplyr::join_by(Parameter, Result, Result_Unit)
    ) %>%
    dplyr::mutate(
      Result = dplyr::if_else(
        is.na(temp_result),
        Result,
        temp_result
      )
    ) %>%
    dplyr::mutate(
      Result_Unit = dplyr::if_else(
        is.na(temp_unit),
        Result_Unit,
        temp_unit
      )
    ) %>%
    dplyr::select(!c(temp_unit, temp_result))

  return(df)
}

#' Convert depth to meters
#'
#' @description Converts columns "Depth", "Depth_Unit" to meters.
#'
#' @param df Dataframe
#'
#' @noRd
depth_to_m <- function(df) {
  if (!"Depth_Unit" %in% colnames(df)) {
    stop("The following column is missing: Depth_Unit", call. = FALSE)
  }

  # Exempt rows
  exempt <- stringr::str_detect(df$Parameter, "Depth")
  exempt <- exempt | skip_rows(df)

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

  exempt <- (df$Depth_Unit %in% c(NA, "m") | exempt)
  if (all(exempt)) {
    return(df)
  }

  exempt <- which(exempt)

  df_temp <- df[-exempt, ] %>%
    dplyr::select(Depth, Depth_Unit) %>%
    unique() %>%
    dplyr::mutate(
      temp_depth = mapply(
        function(x, y) convert_unit(x, y, "m"),
        Depth, Depth_Unit
      )
    )

  df <- dplyr::left_join(df, df_temp, by = dplyr::join_by(Depth, Depth_Unit))
  df <- df %>%
    dplyr::mutate(
      Depth = dplyr::if_else(
        is.na(temp_depth),
        Depth,
        temp_depth
      )
    ) %>%
    dplyr::mutate(
      Depth_Unit = dplyr::if_else(
        is.na(temp_depth),
        Depth_Unit,
        "m"
      )
    ) %>%
    dplyr::select(!temp_depth)

  message("\tConverted depth to meters")
  return(df)
}

#' Assign depth category
#'
#' @description Assigns depth category. Run `depth_to_m` first.
#'
#' @param df Input dataframe.
#' @param sites Site dataframe. Only present for testing purposes.
#'
#' @noRd
assign_depth_category <- function(df, sites = df_sites) {
  if ("Depth_Category" %in% colnames(df)) {
    ok_cat <- c("Surface", "Midwater", "Near Bottom", "Bottom")
    chk <- df$Depth_Category %in% c(NA, ok_cat)
    if (any(!chk)) {
      rws <- which(!chk)
      df[rws, "Depth_Category"] <- NA
      warning("\tRemoved invalid Depth_Category in rows ",
        paste(rws, collapse = ", "), call. = FALSE)
    }
  } else {
    message("\tAdded column Depth_Category")
    df <- dplyr::mutate(df, Depth_Category=NA)
  }

  depth_col <- c("Max_Depth_Surface", "Max_Depth_Midwater",
                 "Max_Depth_Near_Bottom")
  sites <- dplyr::select(sites, dplyr::any_of(c("Site_ID", depth_col)))

  missing_col <- setdiff(depth_col, colnames(sites))
  for (field in missing_col) {
    df <- dplyr::mutate(df, {{field}} := NA)
  }

  df <- dplyr::left_join(
      df, sites,
      by = "Site_ID",
      keep = FALSE
    ) %>%
    dplyr::mutate(
      Max_Depth_Surface = dplyr::if_else(
        is.na(Max_Depth_Surface),
        1,
        Max_Depth_Surface
      )
    ) %>%
    dplyr::mutate(
      Max_Depth_Midwater = dplyr::if_else(
        is.na(Max_Depth_Midwater),
        max(Depth) + 1,
        Max_Depth_Midwater
      )
    ) %>%
    dplyr::mutate(
      Max_Depth_Near_Bottom = dplyr::if_else(
        is.na(Max_Depth_Near_Bottom),
        max(Depth) + 1,
        Max_Depth_Near_Bottom
      )
    ) %>%
    dplyr::mutate(
      Depth_Category = dplyr::case_when(
        !is.na(Depth_Category) ~ Depth_Category,
        is.na(Depth) | Depth_Unit != "m" ~ Depth_Category,
        Depth > Max_Depth_Near_Bottom ~ "Bottom",
        Depth > Max_Depth_Midwater ~ "Near Bottom",
        Depth > Max_Depth_Surface ~ "Midwater",
        TRUE ~ "Surface"
      )
    ) %>%
    dplyr::select(!dplyr::any_of(depth_col))

  return(df)
}

#' list_loc_choices
#'
#' @description Generates list of inputs for radioButton `loc_type`.
#'
#' @param df Input dataframe
#'
#' @return Named list.
#'
#' @noRd
list_loc_choices <- function(df = df_sites) {
  loc_choices = NULL
  # Check for state, town; add "town" option
  if (!is.null(df$Town_Code)) {
    loc_choices <- c("By Town" = "town")
  } else if (!is.null(df$State)) {
    loc_choices <- c("By State" = "town")
  }
  # Check for watershed; add "watershed" OR "blank" option
  if (!is.null(df$Watershed)) {
    loc_choices <- c(loc_choices, "By Watershed" = "watershed")
  } else if (is.null(loc_choices)) {
    loc_choices <- "blank"
  }

  return(loc_choices)
}

#' set_loc_tab
#'
#' @description Sets tab for `tabset_toggle`
#'
#' @param loc_choices Named list.
#'
#' @return Name of tab.
#'
#' @noRd
set_loc_tab <- function(loc_choices) {
  if (length(loc_choices) > 1) {
    return("toggle")
  } else if (loc_choices != "blank") {
    return("notoggle")
  } else {
    return("blank")
  }
}

#' update_town_list
#'
#' @description List towns in selected states.
#'
#' @param df Input dataframe. Default is df_sites - this variable is here for
#'   testing purposes only.
#' @param state_list List of states
#'
#' @return Town_Code list.
#'
#' @noRd
update_town_list <- function(df = df_sites, state_list) {
  if (!"Town_Code" %in% colnames(df)) { return(NULL) }

  town_list <- unique(df$Town_Code) %>%
    sort()

  if (!"State" %in% colnames(df)) { return(town_list) }

  # stringr::str_sub(town_list, -2, -1) works same as sub() but adds dependency
  town_list <- subset(town_list, sub('^.+,\\s+', '', town_list) %in% state_list)

  if (length(town_list) == 0) { return(NULL) }
  return(town_list)
}

#' create_site_list
#'
#' @description Creates list of sites, sorted by name.
#'
#' @param df Input dataframe. Default is df_sites.
#'
#' @return Named site list.
#'
#' @noRd
create_site_list <- function(df = df_sites) {
  req_col <- c("Site_ID", "Site_Name")
  chk <- req_col %in% colnames(df)
  if (any(!chk)) {
    tochk <- req_col[!chk]
    stop("Missing columns ", paste(tochk, collapse = ", "))
  }

  if (nrow(df) == 0) { return(NULL) }

  df <- df %>%
    dplyr::select(Site_ID, Site_Name) %>%
    dplyr::arrange(Site_Name)

  site_list <- df$Site_ID
  names(site_list) <- df$Site_Name

  return(site_list)
}

#' update_site_list
#'
#' @description List sites in selected region.
#'
#' @param df Input dataframe. Default is df_sites - this variable is here for
#'   testing purposes only.
#' @param filter_col Column to filter by.
#' @param filter_list List to filter by.
#'
#' @return Named site list.
#'
#' @noRd
update_site_list <- function(df = df_sites, filter_col, filter_list) {
  if (!filter_col %in% colnames(df)) {
    stop(filter_col, " is an invalid column")
  }

  df <- dplyr::filter(df, .data[[filter_col]] %in% filter_list)

  if (nrow(df) == 0) { return(NULL) }

  site_list <- create_site_list(df)

  return(site_list)
}

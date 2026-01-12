#' Reactable Table
#'
#' @description Formats data as a `reactable` table.
#'
#' @param df Input dataframe.
#' @param show_score Boolean. If TRUE, shows the "score_str" column. If FALSE,
#'  hides the "score_str" column. Default TRUE.
#' @param col_title Title for the "score_num" column. Default value "Average."
#'
#' @return Reactable table.
#'
#' @noRd
reactable_table <- function(df, show_score = TRUE, col_title = "Average") {
  reactable::reactable(
    df,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8"),
      na = "-"
    ),
    columns = column_styles(df, show_score),
    meta = list(col_title = col_title)
  )
}

#' Column Styles
#'
#' @description Helper function that sets column styles for `reactable_table`.
#'
#' @param df Input dataframe.
#' @param show_score Boolean. Whether to show or hide "score_str" column.
#'
#' @return Formatted list of column styles.
#'
#' @noRd
column_styles <- function(df, show_score = TRUE) {
  col_style <- list(
    Site_Name = reactable::colDef(
      rowHeader = TRUE,
      sticky = "left",
      style = list(borderRight = "1px solid #eee")
    )
  )

  col_loc <- c("Town", "County", "State", "Watershed", "Group")
  col_loc <- intersect(colnames(df), col_loc)
  if (length(col_loc) > 0) {
    col_right <- col_loc[length(col_loc)]
    col_style[[col_right]] <- reactable::colDef(
      style = list(borderRight = "1px solid #eee")
    )
  }

  if ("score_num" %in% colnames(df)) {
    col_style[["score_num"]] <- reactable::colDef(
      name = "Value",
      na = "-",
      header = htmlwidgets::JS(
        "function(column, state) {
          const { col_title  } = state.meta
          return col_title
        }"
      )
    )
  }

  if ("score_str" %in% colnames(df)) {
    col_style[["score_str"]] <- reactable::colDef(
      name = "Score",
      show = show_score,
      style = htmlwidgets::JS(
        "function(rowInfo) {
            if (rowInfo.values['score_str'] == 'Excellent') {
              return { backgroundColor: '#afccec' }
            } else if (rowInfo.values['score_str'] == 'Good' |
                rowInfo.values['score_str'] == 'Meets Criteria') {
              return { backgroundColor: '#cbe4e7' }
            } else if (rowInfo.values['score_str'] == 'Fair') {
              return { backgroundColor: '#ffffe0' }
            } else if (rowInfo.values['score_str'] == 'Poor' |
                rowInfo.values['score_str'] == 'Does Not Meet Criteria') {
              return { backgroundColor: '#f9cfb4' }
            } else if (rowInfo.values['score_str'] == 'No Data Available' |
                rowInfo.values['score_str'] == 'No Threshold Established') {
              return { fontStyle: 'italic' }
            }
          }"
      )
    )
  }

  return(col_style)
}

#' Graph Table
#'
#' @description Formats graph data as a `reactable` table.
#'
#' @param df Input dataframe.
#' @param group How to group the data.
#'
#' @return Reactable table.
#'
#' @noRd
graph_table <- function(df, group) {
  col_select <- c("Date", "Result", group)
  df <- dplyr::select(df, dplyr::all_of(col_select))

  var_list <- unique(df[[group]])

  if (length(var_list) == 1) {
    var_name <- df[[group]][1]

    df_wide <- df %>%
      dplyr::select(Date, Result) %>%
      dplyr::rename({{ var_name }} := Result)
  } else {
    df_wide <- df %>%
      tidyr::pivot_wider(
        names_from = {{group}},
        values_from = Result,
        values_fn = list
      )

    for (var in var_list) {
      df_wide <- df_wide %>%
        dplyr::rowwise() %>%
        dplyr::mutate( {{var}} := paste(.data[[var]], collapse=', ')) %>%
        dplyr::ungroup()
    }
  }

  if (group == "Parameter") {
    param <- colnames(df_wide)[2]
    par_unit <- find_unit(param)
    if (par_unit != "") {
      colnames(df_wide)[2] <- paste(param, par_unit)
    }
    if (length(df_wide) > 2) {
      param <- colnames(df_wide)[3]
      par_unit <- find_unit(param)
      if (par_unit != "") {
        colnames(df_wide)[3] <- paste(param, par_unit)
      }
    }
  }

  fig <- reactable::reactable(
    df_wide,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8"),
      na = "-"
    ),
    columns = list(
      Date = reactable::colDef(
        rowHeader = TRUE,
        sticky = "left",
        style = list(borderRight = "1px solid #eee"),
        minWidth = 120
      )
    )
  )

  return(fig)
}

#' Hide Table Columns
#'
#' @description Use javascript to dynamically hide columns in `reactable`
#'   table. Code by dleopold https://github.com/glin/reactable/issues/192
#'
#' @param id Reactable table ID.
#' @param cols List of columns.
#'
#' @noRd
hideCols <- function(id, cols, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("hideCols", list(id = id, cols = cols))
}

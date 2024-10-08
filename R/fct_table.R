#' Reactable Table
#'
#' @description Creates `reactable` table with nicely styled columns.
#'
#' @param df Input dataframe.
#' @param hide_cols List of columns to hide.
#'
#' @return Reactable table.
#'
#' @noRd
reactable_table <- function(df, graph_table = FALSE, show_score = TRUE,
                            par_type = "Average", par_unit = "") {
  reactable::reactable(
    df,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8")
    ),
    columns = column_styles(df, graph_table, show_score),
    meta = list(
      par_type = par_type,
      par_unit = par_unit
    ),
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
column_styles <- function(df, graph_table, show_score = TRUE) {
  if (graph_table) {
    col_style <- list(
      Date = reactable::colDef(
        rowHeader = TRUE,
        sticky = "left",
        style = list(borderRight = "1px solid #eee")))
  } else {
    col_style <- list(
      Site_Name = reactable::colDef(
        rowHeader = TRUE,
        sticky = "left",
        style = list(borderRight = "1px solid #eee")))
  }

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
          const { par_type, par_unit } = state.meta
          return par_type + par_unit
        }")
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

  return (col_style)
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

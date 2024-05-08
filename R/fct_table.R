#' column_styles
#'
#' @description Helper function that sets column styles for `reactable_table`.
#'
#' @param df Input dataframe.
#' @param hide_cols List of columns to hide.
#'
#' @return Formatted list of column styles.
#'
#' @noRd
column_styles <- function(df, hide_cols = NA) {
  col_style <- list(
    Site_Name = reactable::colDef(
      rowHeader = TRUE,
      sticky = "left",
      style = list(borderRight = "1px solid #eee")))

  col_loc <- c("Town", "County", "State", "Watershed", "Group")
  col_loc <- intersect(colnames(df), col_loc)
  if (length(col_loc) > 0) {
    col_right <- col_loc[length(col_loc)]
    col_style[[col_right]] <- reactable::colDef(
      style = list(borderRight = "1px solid #eee")
    )
  }

  if ("score_num" %in% colnames(df) & !"score_num" %in% hide_cols) {
    col_style[["score_num"]] <- reactable::colDef(na = "-")
  }

  if ("score_str" %in% colnames(df)) {
    col_style[["score_str"]] <- reactable::colDef(
        name = "Score",
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
          }"))
  }

  if (all(is.na(hide_cols))) { return (col_style) }

  for (col in hide_cols) {
    col_style[[col]] <- reactable::colDef(show = FALSE)
  }

  return (col_style)
}

#' reactable_table
#'
#' @description Creates `reactable` table with nicely styled columns.
#'
#' @param df Input dataframe.
#' @param hide_cols List of columns to hide.
#'
#' @return Reactable table.
#'
#' @noRd
reactable_table <- function(df, hide_cols = NA) {
  reactable::reactable(
    df,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8")
    ),
    columns = column_styles(df, hide_cols))
}

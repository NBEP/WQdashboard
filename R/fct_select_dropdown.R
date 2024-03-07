#' select_dropdown
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
select_dropdown <- function(id, label, choices, choice_names = NULL,
                            multiple = TRUE, maxOptions = NULL){

  if (!is.null(choices) & !is.null(choice_names)) {
    names(choices) <- choice_names
  }
  choices <- choices[!duplicated(choices)]
  choices <- sort(choices)

  if (is.null(maxOptions)) {
    selected = choices
  } else {
    selected = choices[1]
  }

  shinyWidgets::pickerInput(id,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `selected-text-format` = 'count > 2',
      maxOptions = maxOptions,
      container = "body"),  # Allows dropdown overflow
    multiple = multiple)
}

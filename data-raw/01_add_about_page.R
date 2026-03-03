# Add About Page
#'
#' @description WQdasbhoard includes several customizable pages, including the
#' "About" and "Download" pages.
#'
#' * To update the "About" page, edit `inst/app/www/About.qmd` then run
#' this script. Use visual mode while updating the page to have a preview of
#' how it will look.
#'
#' * To update the "Download" page, edit `inst/app/www/Download.qmd`
#'
#' * To update the overall website style, edit `inst/app/www/_brand.yml`
#' AT MINIMUM, update meta > name
#'
#' @noRd

# CODE ------------------------------------------------------------------------
library(quarto)

quarto::quarto_render("inst/app/www/About.qmd")

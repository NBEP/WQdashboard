#' Add About Page
#'
#' @description WQdasbhoard includes several customizable pages, including the
#' "About" and "Download" pages.
#'
#' There are three files that need to be updated in the `inst/app/www` folder:
#' `About.qmd`, `Download.qmd`, and `_brand.yml`
#'
#' `_brand.yml` sets the site title, colors, and font. Default values have been
#' provided, so feel free to leave most of the values alone. The "meta"
#' section, however, MUST be updated with your organization's name.
#'
#' `About.qmd` and `Download.qmd` allow you to customize the appearance of the
#' "About" and "Download" tabs. These documents use a simple, easy to read
#' markup language called "Quarto", however you may edit them in "Visual Mode"
#' (upper left) if you are unfamiliar with Quarto.
#'
#' @param org_name Name of your organization
#'
#' @noRd

org_name <- ""

# CODE ------------------------------------------------------------------------
library(quarto)

# Render About page
quarto::quarto_render("inst/app/www/About.qmd")
qmd_about <- importwqd::embed_quarto("inst/app/www/About.html")
usethis::use_data(qmd_about, overwrite = TRUE)

# Render Download page
quarto::quarto_render(
  "inst/app/www/Download.qmd",
  execute_params = list(
    org_name = org_name,
    year_updated = format(Sys.time(), "%Y")
  )
)
qmd_download <- importwqd::embed_quarto("inst/app/www/Download.html")
usethis::use_data(qmd_download, overwrite = TRUE)

rm(list = ls(all.names = TRUE))

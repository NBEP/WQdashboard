#' Add Shapefiles - OPTIONAL
#'
#' @description THIS STEP IS OPTIONAL. If you would like to add custom watershed
#' or river layers to the interactive map, use this script to upload shapefiles.
#'
#' Save shapefiles to folder `data-raw/shp` before running this script. Use
#' `CTRL` + `SHIFT` + `ENTER` to run the script.
#'
#' @param watershed_shp Name of the watershed shapefile. Shapefile must be a
#' polygon layer.
#' @param watershed_name_col Field name for watershed shapefile. This field will
#' be used to label each polygon.
#' @param river_shp Name of river shapefile. Shapefile must be a polyline layer.
#' @param river_name_col Field name for river shapefile. This field will be
#' used to label each polyline.
#'
#' @noRd

# SHAPEFILE - Watershed Boundaries
watershed_shp <- NA
watershed_name_col <- "Field"

# SHAPEFILE - Rivers
river_shp <- NA
river_name_col <- "Field"

# CODE - DO NOT EDIT BELOW THIS LINE -------------------------------------------
library(sf)
library(dplyr)
library(tidyr)

# Import shapefiles ----
if (!is.null(watershed_shp) && !is.na(watershed_shp)) {
  shp_watershed <- sf::read_sf(dsn = "data-raw/shp", layer = watershed_shp) |>
    dplyr::select(!!watershed_name_col)
  colnames(shp_watershed)[1] <- "Label"

  usethis::use_data(shp_watershed, overwrite = TRUE)
}

if (!is.null(river_shp) && !is.na(river_shp)) {
  shp_river <- sf::read_sf(dsn = "data-raw/shp", layer = river_shp) |>
    dplyr::select(!!river_name_col)
  colnames(shp_river)[1] <- "Label"

  usethis::use_data(shp_river, overwrite = TRUE)
}

rm(list = ls(all.names = TRUE))

# Go to next page
rstudioapi::navigateToFile("data-raw/03_add_sites.R")

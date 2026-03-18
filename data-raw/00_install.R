# Install packages
#
# Run this script to install any missing packages. This will also install
# `tinytex`, which is used to generate PDF files.
#
# This script only needs to be run once, but MUST be run if you are unfamiliar
# with R and don't know how to install packages.
#
# To run this script and all following scripts, use CTRL + SHIFT + ENTER
#
# DO NOT EDIT ANYTHING BELOW "Code ------"

# Code ------------------------------------------------------------------------
# Install packages ----
if (!require(renv)) {
  install.packages("renv")
}

library(renv)
renv::install(exclude = c("WQdashboard", "covrpage"))

# Add tinytex ----
if (!require(tinytex)) {
  install.packages('tinytex')
}

library(tinytex)
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}

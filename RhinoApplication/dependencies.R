# This file allows packrat (used by rsconnect during deployment) to pick up dependencies.
library(dplyr)
library(echarts4r)
library(htmlwidgets)
library(reactable)
library(rhino)
library(tidyr)
library(shinyWidgets)
library(plotly)
library(glue)
library(bslib)
library(stats)
library(shinycssloaders)
library(DT)
library(upsetjs)
library(tidyverse)
library(DT)
library(upsetjs)
library(ReactomePA)
library(rrvgo)
library(clusterProfiler)
library(rsconnect)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("clusterProfiler")
BiocManager::install("ReactomePA")


packages_to_install <- c(
  "dplyr", "echarts4r", "htmlwidgets", "reactable", "rhino",
  "tidyr", "shinyWidgets", "plotly", "glue", "bslib",
  "shinycssloaders", "DT", "upsetjs", "tidyverse", "DT",
  "upsetjs", "ReactomePA", "rrvgo"
)

# Install packages if not already installed
install_if_missing <- function(pkg) {
  if (!(pkg %in% installed.packages()[,"Package"])) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(sapply(packages_to_install, install_if_missing))

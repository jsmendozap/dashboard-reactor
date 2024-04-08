#!/usr/bin/env Rscript

cat("Verifying and installing required packages\n")
if (!"pacman" %in% installed.packages()) { install.packages("pacman", repos = 'http://cran.us.r-project.org') }

pacman::p_load(
    tidyverse, readxl, janitor, DT, dygraphs,
    bs4Dash, shiny, prettyunits, plotly
)

cat("Running application\n")
walk(.x = c("src/ui.R", "src/server.R"), .f = source)
shinyApp(ui, server)

#!/usr/bin/env Rscript

cat("Verifying and installing required packages\n")
if (!"pacman" %in% installed.packages()) { install.packages("pacman", repos = 'http://cran.us.r-project.org') }

pacman::p_load(
    tidyverse, readxl, janitor, DT, dygraphs, shinyWidgets,
    bs4Dash, shiny, prettyunits, plotly, zoo 
)

cat("Running application\n")
walk(.x = dir('src', '*.R', full.names = T, recursive = T), .f = source)
shinyApp(ui, server)

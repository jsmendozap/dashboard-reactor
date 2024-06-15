#!/usr/bin/env Rscript

cat("Verifying and installing required packages\n")
if (!"pacman" %in% installed.packages()) { install.packages("pacman", repos = 'http://cran.us.r-project.org') }

pacman::p_load(
    plyr, tidyverse, readxl, janitor, dygraphs, shinyWidgets, bs4Dash, shiny,
    prettyunits, plotly, zoo, reactable, reactable.extras,
    shinyFiles, quarto 
)

cat("Running application\n")
walk(.x = dir('src', '*.R', full.names = T, recursive = T), .f = source)
shinyApp(ui, server)

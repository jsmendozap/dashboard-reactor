#!/usr/bin/env Rscript

cat("Verifying and installing required packages\n")
if (!"pacman" %in% installed.packages()) { install.packages("pacman", repos = 'http://cran.us.r-project.org') }

pacman::p_load(
    quarto, plyr, tidyverse, readxl, janitor, dygraphs, shinyWidgets, bs4Dash,
    shiny, prettyunits, plotly, zoo, reactable, reactable.extras, shinyFiles,
    duckdb, DBI, shinyToastify, prompter
)

if (.Platform$OS.type == "windows") {
  args <- commandArgs(trailingOnly = TRUE)
  Sys.setenv(QUARTO_PATH = args[1])
}

cat("Running application\n")
purrr::walk(.x = dir('src', '*.R', full.names = T, recursive = T), .f = source)
db_setup()
shiny::shinyApp(ui, server)

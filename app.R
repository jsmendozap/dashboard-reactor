#!/usr/bin/env Rscript

if (!"pak" %in% installed.packages()) { install.packages("pak", repos = 'http://cran.us.r-project.org') }

cat("Verifying and installing required packages\n")
lib <- c("tidyverse", "readxl", "janitor", "DT", "dygraphs", "bs4Dash", "shiny", "prettyunits", "plotly")
lapply(lib, \(x) { 
    if(rlang::is_installed(x)) {
        library(x, character.only = T, quietly = T)
    } else {
        pak::pkg_install(x)
        library(x, character.only = T, quietly = T)
    }
})

cat("Running application\n")
walk(.x = c("src/ui.R", "src/server.R"), .f = source)
shinyApp(ui, server)

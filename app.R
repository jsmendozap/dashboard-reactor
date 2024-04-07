#!/usr/bin/env Rscript

if (!"pak" %in% installed.packages()) { install.packages("pak", repos = 'http://cran.us.r-project.org') }

cat("Veryfing and installing required packages")
lib <- c(tidyverse, readxl, janitor, DT, dygraphs, bs4Dash, shiny, prettyunits, plotly)
lapply(lib, \(x) { 
    if(rlang::is_installed(x)) {
        library(x)
    } else {
        pak::pkg_install(x)
        library(x)
    }
}

cat("Running application)
walk(.x = c("src/ui.R", "src/server.R"), .f = source)
shinyApp(ui, server)

#!/usr/bin/env Rscript

cat("Verifying and installing required packages\n")
if (!"pak" %in% installed.packages()) { install.packages("pak", repos = 'http://cran.us.r-project.org') }

lib <- c("tidyverse", "readxl", "janitor", "DT", "dygraphs", "bs4Dash", "shiny", "prettyunits", "plotly")
sapply(lib, \(x) { 
    if(rlang::is_installed(x)) {
        require(x, character.only = T, quietly = T, warn.conflicts = F)
    } else {
        pak::pkg_install(x)
        require(x, character.only = T, quietly = T, warn.conflicts = F)
    }
})

cat("Running application\n")
walk(.x = c("src/ui.R", "src/server.R"), .f = source)
shinyApp(ui, server)

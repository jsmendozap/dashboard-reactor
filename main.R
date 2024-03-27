#!/usr/bin/env Rscript

if (!"pacman" %in% installed.packages()) {install.packages('pacman')}

pacman::p_load(tidyverse, readxl, janitor, DT, dygraphs,
               bs4Dash, shiny, prettyunits)

walk(.x = c("ui.R", "server.R"), .f = source)
shinyApp(ui, server) %>% runApp(port = 3025)
#!/usr/bin/env Rscript

if (!"pacman" %in% installed.packages()) {install.packages('pacman')}

pacman::p_load(tidyverse, readxl, janitor, DT, dygraphs,
               bs4Dash, shiny, prettyunits, plotly)

walk(.x = c("src/ui.R", "src/server.R"), .f = source)
shinyApp(ui, server) #%>% runApp(port = 3026)

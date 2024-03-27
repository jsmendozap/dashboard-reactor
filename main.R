if (!"pacman" %in% installed.packages()) {install.packages('pacman')}

pacman::p_load(tidyverse, readxl, janitor, DT, dygraphs,
               bs4Dash, shiny, prettyunits)

purrr::walk(.x = c("ui.R", "server.R"), .f = source)
shinyApp(ui, server, options = list(launch.browser = T))
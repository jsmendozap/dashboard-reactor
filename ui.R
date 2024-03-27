source("Pages/log.R")
source("Pages/quality.R")

menu <- list(
  menuItem("Log summary", tabName = 'log', icon = icon("clipboard-list", style = "margin-right: 5px")),
  menuItem("Quality control", tabName = 'quality', icon = icon("stopwatch", style = "margin-right: 5px"))
)

ui <- dashboardPage(
  dashboardHeader(title = "Experiment control"),
  dashboardSidebar(minified = F, sidebarMenu(menu)),
  dashboardBody(tabItems(log, quality))
)
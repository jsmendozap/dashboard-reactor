source("src/pages/log.R")
source("src/pages/quality.R")

menu <- list(
  fileInput("file", "Reactor file"),
  menuItem("Log summary", tabName = "log", icon = icon("clipboard-list", style = "margin-right: 5px")),
  menuItem("Quality control", tabName = "quality", icon = icon("stopwatch", style = "margin-right: 5px"))
)

ui <- dashboardPage(
  dashboardHeader(title = dashboardBrand(
    image = "https://media.licdn.com/dms/image/D4E0BAQFRTe4HelmdIA/company-logo_200_200/0/1706107028417/dusselier_lab_logo?e=1720051200&v=beta&t=sfFl9vN4z9mXj411z83IRsH6KmaWfuxWYfro3K2a218",
    title = span("Experiment control", style = "font-weight: bold; color: #6c757d")
  )),
  dashboardSidebar(
    collapsed = F, minified = F, sidebarMenu(menu),
    status = "info", elevation = 2
  ),
  dashboardBody(tabItems(log, quality))
)

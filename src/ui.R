menu <- list(
      br(),
      shinyDirButton("directory", "Select folder",
                     "Please select folder containing reactor, gc and ms files",
                     icon = icon('folder', style = 'margin-right: 5px'),
                     style = 'background-color: white'),
      sliderInput(inputId = 'log_events',
                  label = "Event's minimum duration",
                  min = 0, max = 60, value = 0),
      menuItem("Log summary", tabName = "log", icon = icon("book", style = "margin-right: 5px")),
      menuItem("Quality control", tabName = "quality", icon = icon("stopwatch", style = "margin-right: 5px")),
      menuItem("Raw data", tabName = "raw-data", icon = icon("file-lines", style = "margin-right: 5px")),
      menuItem("Chemometric", tabName = "chemometric", icon = icon("calculator", style = "margin-right: 5px")),  
      br(), downloadButton('report', 'Generate report',
                           style = 'background-color: white',
                           icon = icon('download', style = 'margin-right: 5px'))
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
  dashboardBody(tabItems(log, quality, raw_data, chemometric),
                tags$head(includeCSS("www/custom.css")),
                reactable_extras_dependency())
)

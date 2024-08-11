menu <- list(
      br(),
      shinyDirButton("directory", "Select folder",
                     "Please select folder containing reactor, gc and ms files",
                     icon = icon('folder', style = 'margin-right: 5px'),
                     style = 'background-color: white'),
      span("Event's minimum duration", style = 'padding-bottom: 0px; font-weight: bold; display: flex; justify-content: center'),
      div(
        numericInput(inputId = 'log_events', 
                   label = NULL,
                   min = 0, max = 1440, value = 0, width = 150),
        span('minutes', sytle = 'margin-left: 5px'),
        style = 'display: flex; align-items: center; margin-top: 0'
      ),               
      menuItem("Reaction settings", tabName = "reaction", icon = icon("flask-vial", style = "margin-right: 5px")),
      menuItem("Log summary", tabName = "log", icon = icon("book", style = "margin-right: 5px")),
      menuItem("Quality control", tabName = "quality", icon = icon("stopwatch", style = "margin-right: 5px")),
      menuItem("Raw data", tabName = "raw-data", icon = icon("file-lines", style = "margin-right: 5px")),
      menuItem("Chemometric", tabName = "chemometric", icon = icon("calculator", style = "margin-right: 5px")),  
      br(), downloadButton('report', 'Generate report',
                           style = 'background-color: white',
                           icon = icon('download', style = 'margin-right: 5px'))
  )

ui <- dashboardPage(title = 'Experiment control',
  dashboardHeader(title = dashboardBrand(
    image = "https://www.dusselier-lab.org/uploads/media/cache/default/uploads/c1e65122e588fa60c0dabe8f2168cc4e.jpeg",
    title = span("Experiment control", style = "font-weight: bold; color: #6c757d")),
    div(textOutput('dir_header'), style = 'color: #6c757d; font-weight: bold')
  ),
  dashboardSidebar(
    collapsed = F, minified = F, sidebarMenu(menu),
    status = "info", elevation = 2
  ),
  dashboardBody(tabItems(reaction_ui('reaction'), log_ui('log'), quality_ui('quality'),
                         raw_data_ui('raw'), chemometric_ui('chem')),
                tags$head(includeCSS("www/custom.css")), 
                reactable_extras_dependency())
)

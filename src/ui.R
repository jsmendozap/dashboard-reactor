bs4DashTheme <- fresh::bs4Dash_theme(
  info = "#484e68",
  "main-bg" = "#ffffff",
  "sidebar-light-bg" = "#fdf0f0",
  "sidebar-light-header_color" = "black"
)

menu <- list(
  shiny::br(),
  shinyFiles::shinyDirButton(
    "directory",
    "Select folder",
    "Please select folder containing reactor, gc and ms files",
    icon = shiny::icon('folder', style = 'margin-right: 5px'),
    style = 'background-color: white'
  ),
  shiny::span(
    "Event's minimum duration",
    style = 'padding-bottom: 0px; font-weight: bold; display: flex; justify-content: center'
  ),
  shiny::div(
    shiny::numericInput(
      inputId = 'log_events',
      label = NULL,
      min = 0,
      max = 1440,
      value = 0,
      width = 150
    ),
    shiny::span('minutes', sytle = 'margin-left: 5px'),
    style = 'display: flex; align-items: center; margin-top: 0'
  ),
  bs4Dash::menuItem(
    "Reaction settings",
    tabName = "reaction",
    icon = shiny::icon("flask-vial", style = "margin-right: 5px")
  ),
  bs4Dash::menuItem(
    "Catalyst",
    tabName = "catalyst",
    icon = shiny::icon("atom", style = "margin-right: 5px")
  ),
  bs4Dash::menuItem(
    "Log summary",
    tabName = "log",
    icon = shiny::icon("book", style = "margin-right: 5px")
  ),
  bs4Dash::menuItem(
    "Quality control",
    tabName = "quality",
    icon = shiny::icon("stopwatch", style = "margin-right: 5px")
  ),
  bs4Dash::menuItem(
    "Raw data",
    tabName = "raw-data",
    icon = shiny::icon("square-poll-vertical", style = "margin-right: 5px")
  ),
  bs4Dash::menuItem(
    "Chemometric",
    tabName = "chemometric",
    icon = shiny::icon("calculator", style = "margin-right: 5px")
  ),
  shiny::br(),
  shiny::downloadButton(
    'report',
    'Generate report',
    style = 'background-color: white',
    icon = shiny::icon('download', style = 'margin-right: 5px')
  )
)

ui <- bs4Dash::dashboardPage(
  title = 'Experiment control',
  bs4Dash::dashboardHeader(
    fixed = T,
    title = bs4Dash::dashboardBrand(
      image = "https://www.dusselier-lab.org/uploads/media/cache/default/uploads/c1e65122e588fa60c0dabe8f2168cc4e.jpeg",
      title = shiny::span(
        "Experiment control",
        style = "font-weight: bold; color: #ffffff"
      )
    ),
    shiny::div(
      shiny::textOutput('dir_header'),
      style = 'color: #ffffff; font-weight: bold'
    )
  ),
  bs4Dash::dashboardSidebar(
    collapsed = F,
    minified = F,
    bs4Dash::sidebarMenu(menu),
    status = "info",
    elevation = 1
  ),
  bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      reaction_ui('reaction'),
      catalyst_ui('catalyst'),
      log_ui('log'),
      quality_ui('quality'),
      raw_data_ui('raw'),
      chemometric_ui('chem')
    ),
    fresh::use_theme(bs4DashTheme),
    tags$head(shiny::includeCSS("www/custom.css")),
    reactable.extras::reactable_extras_dependency()
  )
)

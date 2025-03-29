#' UI Components for the Dashboard
#'
#' @description This file contains UI functions for the dashboard components, including the header, sidebar, and body.
#'
#' @importFrom bs4Dash dashboardHeader dashboardBrand dashboardSidebar sidebarMenu dashboardBody tabItems menuItem
#' @importFrom shiny span div textOutput br icon numericInput downloadButton
#' @importFrom shinyFiles shinyDirButton
#' @importFrom fresh use_theme bs4Dash_theme
#'
#' @noRd

header_ui <- function() {
  dashboardHeader(
    fixed = TRUE,
    title = dashboardBrand(
      image = "https://www.dusselier-lab.org/uploads/media/cache/default/uploads/c1e65122e588fa60c0dabe8f2168cc4e.jpeg",
      title = span(
        "Experiment control",
        style = "font-weight: bold; color: #ffffff"
      )
    ),
    div(
      textOutput('dir_header'),
      style = 'color: #ffffff; font-weight: bold'
    )
  )
}

sidebar_ui <- function() {
  dashboardSidebar(
    collapsed = FALSE,
    minified = FALSE,
    sidebarMenu(menu_components()),
    status = "info",
    elevation = 1
  )
}

body_ui <- function() {
  dashboardBody(
    tabItems(
      mod_reaction_ui('reaction'),
      mod_catalyst_ui('catalyst'),
      mod_log_ui('log'),
      mod_quality_ui('quality'),
      mod_raw_ui('raw'),
      mod_chemometrics_ui('chem')
    ),
    use_theme(bs4DashTheme)
  )
}

bs4DashTheme <- bs4Dash_theme(
  info = "#484e68",
  "main-bg" = "#ffffff",
  "sidebar-light-bg" = "#fdf0f0"
)

menu_components <- function() {
  list(
    br(),
    shinyDirButton(
      "directory",
      "Select folder",
      "Please select folder containing reactor, gc and ms files",
      icon = icon('folder', style = 'margin-right: 5px'),
      style = 'background-color: white'
    ),
    span(
      "Event's minimum duration",
      style = 'padding-bottom: 0px; font-weight: bold; display: flex; justify-content: center'
    ),
    div(
      numericInput(
        inputId = 'log_events',
        label = NULL,
        min = 0,
        max = 1440,
        value = 0,
        width = 150
      ),
      span('minutes', style = 'margin-left: 5px'),
      style = 'display: flex; align-items: center; margin-top: 0'
    ),
    menuItem(
      "Reaction settings",
      tabName = "reaction",
      icon = icon("flask-vial", style = "margin-right: 5px")
    ),
    menuItem(
      "Catalyst",
      tabName = "catalyst",
      icon = icon("atom", style = "margin-right: 5px")
    ),
    menuItem(
      "Log summary",
      tabName = "log",
      icon = icon("book", style = "margin-right: 5px")
    ),
    menuItem(
      "Quality control",
      tabName = "quality",
      icon = icon("stopwatch", style = "margin-right: 5px")
    ),
    menuItem(
      "Raw data",
      tabName = "raw-data",
      icon = icon("square-poll-vertical", style = "margin-right: 5px")
    ),
    menuItem(
      "Chemometric",
      tabName = "chemometric",
      icon = icon("calculator", style = "margin-right: 5px")
    ),
    br(),
    downloadButton(
      'report',
      'Generate report',
      style = 'background-color: white',
      icon = icon('download', style = 'margin-right: 5px')
    )
  )
}

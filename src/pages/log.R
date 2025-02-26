choices <- c(
  "Air flow" = "fi_110",
  "CO2 flow" = "fi_120",
  "Argon/Propane flow" = "fi_130",
  "Nitrogen flow" = "fi_140",
  "Setted pressure" = "p_set",
  "Measured temperature" = "tic_300_pv",
  "Setted temperature" = "tic_300_sp"
)

log_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabItem(
    tabName = 'log',
    shiny::fluidRow(
      bs4Dash::column(
        width = 8,
        style = "padding-right: 0px",
        bs4Dash::box(
          title = "Events summary",
          status = 'info',
          solidHeader = T,
          width = 12,
          reactable::reactableOutput(ns('log')),
          style = 'padding: 10px'
        )
      ),
      bs4Dash::column(
        width = 4,
        bs4Dash::box(
          title = "Leak test",
          width = 12,
          status = 'info',
          solidHeader = T,
          shiny::uiOutput(ns('leak')),
          style = "padding: 10px"
        ),
        bs4Dash::box(
          title = "Reactor's inflow",
          status = 'info',
          solidHeader = T,
          width = 12,
          shiny::textOutput(ns('valve')),
          shiny::br(),
          shiny::fluidRow(
            bs4Dash::column(
              width = 6,
              bs4Dash::valueBoxOutput(ns("fi_110"), width = 12),
              bs4Dash::valueBoxOutput(ns("fi_120"), width = 12),
              style = 'padding: 0px'
            ),
            bs4Dash::column(
              width = 6,
              bs4Dash::valueBoxOutput(ns("fi_130"), width = 12),
              bs4Dash::valueBoxOutput(ns("fi_140"), width = 12),
              style = 'padding: 0px'
            )
          ),
          style = 'padding-right: 8px'
        )
      )
    ),
    bs4Dash::box(
      status = "info",
      title = "Experiment overview",
      solidHeader = T,
      width = 12,
      shiny::fluidRow(
        bs4Dash::column(
          width = 4,
          shinyWidgets::multiInput(
            inputId = ns('var'),
            label = "Variables to plot",
            choices = choices,
            selected = choices[1],
            width = "100%",
            options = list(limit = 2)
          ),
          shiny::div(
            id = 'legend',
            style = "margin-left: 5px; margin-bottom: 20px"
          )
        ),
        bs4Dash::column(
          width = 8,
          dygraphs::dygraphOutput(ns("dygraph"), height = '45vh')
        )
      )
    )
  )
}

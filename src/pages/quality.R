quality_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabItem(
    tabName = 'quality',
    bs4Dash::tabBox(
      id = "tabset1",
      height = "100%",
      width = 12,
      shiny::tabPanel(
        title = "Flow",
        shiny::fluidRow(
          bs4Dash::tabBox(
            width = 7,
            shiny::tabPanel(
              title = "Rate Flow",
              reactable::reactableOutput(ns('flow'))
            ),
            shiny::tabPanel(
              title = "Correlations",
              reactable::reactableOutput(ns('corr'))
            )
          ),
          bs4Dash::box(
            title = "Normalized milimeters plot",
            status = 'info',
            solidHeader = T,
            width = 5,
            plotly::plotlyOutput(ns('norm'))
          )
        )
      ),
      shiny::tabPanel(
        title = "Temperature",
        shiny::fluidRow(
          bs4Dash::box(
            title = 'Summary',
            width = 12,
            status = 'info',
            solidHeader = T,
            reactable::reactableOutput(ns('temp'))
          ),
          plotly::plotlyOutput(ns('diffTemp'), height = '100vh')
        )
      ),
      shiny::tabPanel(
        title = "Pressure",
        shiny::fluidRow(
          bs4Dash::box(
            width = 7,
            status = 'info',
            solidHeader = T,
            reactable::reactableOutput(ns('press'))
          ),
          bs4Dash::box(
            width = 5,
            status = 'info',
            solidHeader = T,
            dygraphs::dygraphOutput(ns('plotPress')),
            title = 'Pressure difference per event plot'
          )
        )
      )
    )
  )
}

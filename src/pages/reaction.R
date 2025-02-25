reaction_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabItem(
    tabName = 'reaction',
    shiny::fluidRow(
      bs4Dash::box(
        title = "Available reactions",
        width = 5,
        status = 'info',
        solidHeader = T,
        shiny::uiOutput(ns("reactions_table")),
        shiny::uiOutput(ns("reaction_desc"))
      ),
      bs4Dash::box(
        title = "Reaction Details",
        width = 7,
        status = 'info',
        solidHeader = T,
        reactable::reactableOutput(ns('setting'))
      )
    )
  )
}

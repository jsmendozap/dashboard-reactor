catalyst_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabItem(
    tabName = 'catalyst',
    shiny::fluidRow(
      bs4Dash::box(
        title = "Catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = T,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::textAreaInput(
              inputId = ns("md"),
              label = "Description of the material"
            )
          ),
          shiny::column(
            width = 6,
            shiny::textAreaInput(
              inputId = ns("pd"),
              label = "Preparation description"
            )
          )
        ),
        excelR::excelOutput(ns('cc'), height = "100%")
      ),
      bs4Dash::box(
        title = "Fresh catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = T,
        collapsed = T,
        excelR::excelOutput(ns('fcc'), width = "100%", height = "100%")
      ),
      bs4Dash::box(
        title = "Spent catalyst characterization",
        width = 12,
        status = 'info',
        solidHeader = T,
        collapsed = T,
        excelR::excelOutput(ns('scc'), width = "100%", height = "100%")
      )
    )
  )
}

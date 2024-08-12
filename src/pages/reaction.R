reaction_ui <- function(id){

  ns <- shiny::NS(id)
  choices <- c("Reactant", "Product", "IS")

  bs4Dash::tabItem(tabName = 'reaction',
          bs4Dash::tabBox(width = 12,
                shiny::tabPanel(title = "Create setting",
                        shiny::fluidRow(
                          shiny::div(
                            bs4Dash::column(width = 4, shiny::textInput(ns('compound'), 'Compound')),
                            bs4Dash::column(width = 2, shinyWidgets::pickerInput(ns('c_type'), 'Type', choices)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc1"), "MFC 1", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc2"), "MFC 2", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc3"), "MFC 3", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc4"), "MFC 4", value = NA)),
                            bs4Dash::column(width = 2, bs4Dash::actionButton(ns("new"), "Add row")),
                            style = "display: flex; align-items: center"
                          ),
                          reactable::reactableOutput(ns('compounds_table')),
                        ),
                        shiny::br(),
                        shinyWidgets::searchInput(
                          inputId = ns("save"),
                          label = "Save setting",
                          placeholder = "Setting name",
                          btnSearch = shiny::icon("floppy-disk"),
                          width = "450px"
                        )
                ),
                shiny::tabPanel(title = "Load Setting"
                )
          )
        )
}

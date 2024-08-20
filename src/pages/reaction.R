reaction_ui <- function(id){

  ns <- shiny::NS(id)
  choices <- c("Reactant", "Product", "IS")

  bs4Dash::tabItem(tabName = 'reaction',
          bs4Dash::tabBox(width = 12,
                shiny::tabPanel(title = "Create reaction",
                        shiny::fluidRow(
                          shiny::div(
                            bs4Dash::column(width = 2, shinyWidgets::pickerInput(ns('compound'), 'Compound', compounds$name,
                                                                                 options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                            bs4Dash::column(width = 2, shinyWidgets::pickerInput(ns('c_type'), 'Type', choices)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc1"), "CMP 1", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc2"), "CMP 2", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc3"), "CMP 3", value = NA)),
                            bs4Dash::column(width = 1, shiny::numericInput(ns("mfc4"), "CMP 4", value = NA)),
                            div(
                              bs4Dash::column(width = 6, bs4Dash::actionButton(ns("new"), "Add")),
                              bs4Dash::column(width = 6, bs4Dash::actionButton(ns("remove"), "Remove")),
                              style = "display: flex; text-align: center"
                            ),
                            style = "display: flex; align-items: center"
                          ),
                          reactable::reactableOutput(ns('compounds_table')),
                        ),
                        shiny::br(),
                        shinyWidgets::searchInput(
                          inputId = ns("save"),
                          label = "Save reaction",
                          placeholder = "Reaction name",
                          btnSearch = shiny::icon("floppy-disk"),
                          width = "450px"
                        )
                ),
                shiny::tabPanel(title = "Load reaction",
                        shiny::fluidRow(
                          bs4Dash::column(width = 3, 
                                          shiny::uiOutput(ns("reactions_table"))
                          ),
                          bs4Dash::column(width = 9, 
                                          reactable::reactableOutput(ns('setting'))
                          )
                        )        
                )
          )
        )
}

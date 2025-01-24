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
                            prompter::add_prompt(
                              bs4Dash::column(width = 1, shiny::numericInput(ns("mfc1"),
                                              label = HTML(paste0("% y",tags$sub("1"))), value = NA)),
                              position = "top", message = "Composition of MFC 1", rounded = T, arrow = F
                            ),
                            prompter::add_prompt(
                              bs4Dash::column(width = 1, shiny::numericInput(ns("mfc2"),
                                              label = HTML(paste0("% y",tags$sub("2"))), value = NA)),
                              position = "top", message = "Composition of MFC 2", rounded = T, arrow = F
                            ),
                            prompter::add_prompt(
                              bs4Dash::column(width = 1, shiny::numericInput(ns("mfc3"),
                                              label = HTML(paste0("% y",tags$sub("3"))), value = NA)),
                              position = "top", message = "Composition of MFC 3", rounded = T, arrow = F
                            ),
                            prompter::add_prompt(
                              bs4Dash::column(width = 1, shiny::numericInput(ns("mfc4"),
                                              label = HTML(paste0("% y",tags$sub("4"))), value = NA)),
                              position = "top", message = "Composition of MFC 4", rounded = T, arrow = F
                            ),
                            div(
                              bs4Dash::column(width = 6, bs4Dash::actionButton(ns("new"), "Add")),
                              bs4Dash::column(width = 6, bs4Dash::actionButton(ns("remove"), "Remove")),
                              style = "display: flex; text-align: center"
                            ),
                            style = "display: flex; align-items: center"
                          )
                        ),
                        shiny::br(),
                        shiny::fluidRow(
                          reactable::reactableOutput(ns('compounds_table'))
                        ),
                        shiny::br(),
                        shiny::fluidRow(
                          shiny::column(width = 4, shiny::uiOutput(ns('templ')))
                        ),
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
                          shiny::div(width = 3,
                            bs4Dash::column(width = 12, shiny::uiOutput(ns("reactions_table"))),
                            bs4Dash::column(width = 12, bs4Dash::actionButton(ns("delete"), "Delete setting"))
                          ),
                          bs4Dash::column(width = 9, reactable::reactableOutput(ns('setting'))
                          )
                        )        
                )
          )
        )
}

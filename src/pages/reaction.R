reaction_ui <- function(id){

  ns <- NS(id)
  choices <- c("Reactant", "Product", "IS")

  tabItem(tabName = 'reaction',
          tabBox(width = 12, 
                tabPanel(title = "Create setting",
                        fluidRow(
                          div(
                            column(width = 4, textInput(ns('compound'), 'Compound')),
                            column(width = 2, pickerInput(ns('c_type'), 'Type', choices)),
                            column(width = 1, numericInput(ns("mfc1"), "MFC 1", value = NA)),
                            column(width = 1, numericInput(ns("mfc2"), "MFC 2", value = NA)),
                            column(width = 1, numericInput(ns("mfc3"), "MFC 3", value = NA)),
                            column(width = 1, numericInput(ns("mfc4"), "MFC 4", value = NA)),
                            column(width = 2, actionButton(ns("new"), "Add row")),
                            style = "display: flex; align-items: center"               
                          ),
                          reactableOutput(ns('compounds_table')),
                        ),
                        br(),
                        searchInput(
                          inputId = ns("save"),
                          label = "Save setting", 
                          placeholder = "Setting name",
                          btnSearch = icon("floppy-disk"), 
                          width = "450px"
                        )
                ),
                tabPanel(title = "Load Setting"
                )      
          )
        )
}
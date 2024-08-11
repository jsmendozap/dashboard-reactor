reaction_ui <- function(id){

  ns <- NS(id)

  tabItem(tabName = 'reaction',
                fluidRow(
                  box(title = "Stoichiometric reaction settings",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       searchInput(
                         inputId = ns("formula"),
                         label = "Please write the reaction", 
                         placeholder = "Propileno + co2",
                         btnSearch = icon("search"), 
                         btnReset = icon("remove"),
                         width = "100%"
                      ),
                      uiOutput(ns('descrip'))
                  )
                )
        )
}
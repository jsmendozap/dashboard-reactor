quality_ui <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = 'quality',
          tabBox(
            id = "tabset1", height = "100%", width = 12,
            tabPanel(title = "Flow", 
                    fluidRow( 
                      tabBox(width = 7,
                        tabPanel(title = "Rate Flow", reactableOutput(ns('flow'))),
                        tabPanel(title = "Correlations", reactableOutput(ns('corr')))
                        ),
                      box(title = "Normalized milimeters plot",
                          status = 'info',
                          solidHeader = T,
                          width = 5,
                          plotlyOutput(ns('norm'))
                        )
                      )
                    ),
            tabPanel(title = "Temperature",
                    fluidRow(
                      box(title = 'Summary', width = 12, status = 'info',
                          solidHeader = T, reactableOutput(ns('temp'))),
                      plotlyOutput(ns('diffTemp'), height = '100vh')
                      )
                    ),
            tabPanel(title = "Pressure",
                    fluidRow(
                      box(width = 7, status = 'info', solidHeader = T, reactableOutput(ns('press'))),
                      box(width = 5, status = 'info', solidHeader = T,
                          dygraphOutput(ns('plotPress')), title = 'Pressure difference per event plot')
                      )
                    )
          )
      )
}
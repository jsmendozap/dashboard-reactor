raw_data_ui <- function(id) {
       
       ns <- NS(id)
       
       tabItem(tabName = "raw-data",
              tabBox(
              id = "tabset2", height = "100%", width = 12,
              tabPanel(title = "GC composition",
                     fluidRow(
                            column(width = 3,
                                   uiOutput(ns('UIcompunds')),
                                   uiOutput(ns('gc_events')),
                                   uiOutput(ns('xgc'))),
                            column(width = 9, 
                                   plotlyOutput(ns('composition'), height = '100vh'))
                            )
                     ),
              tabPanel(title = "MS signals",
                     fluidRow(
                            column(width = 3,
                                   uiOutput(ns('xms')),
                                   uiOutput(ns('yms')),
                                   uiOutput(ns('ms_events')),
                                   sliderInput(ns('smooth'), 'Smoothing level:',
                                          min = 0, max = 1, value = 0),
                                   uiOutput(ns("ms_int")),
                                   plotOutput(ns('int_plot'), height = '250px')
                                   ),
                            tabBox(
                            id = "tabset3", height = "100%", width = 9,
                            tabPanel(title = "General", 
                                   fluidRow(
                                   column(width = 12,
                                          dygraphOutput(ns('msplot_gen'), height = '450px'))
                                   )
                                   ),
                            tabPanel(title = "Events", 
                                   fluidRow(
                                   column(width = 12,
                                          plotlyOutput(ns('msplot'), height = '100vh'))
                                   )
                            )
                            )
                            )
                     )
              )
              )
}       
raw_data <- tabItem(tabName = "raw-data",
                       tabBox(
                         id = "tabset2", height = "100%", width = 12,
                         tabPanel(title = "GC composition",
                                  fluidRow(
                                    column(width = 3,
                                           uiOutput('UIcompunds'),
                                           uiOutput('gc_events'),
                                           uiOutput('xgc')),
                                    column(width = 9, 
                                           plotlyOutput('composition', height = '150vh')))
                                  ),
                         tabPanel(title = "MS signals",
                                  fluidRow(
                                    column(width = 3,
                                           uiOutput('xms'),
                                           uiOutput('yms'),
                                           sliderInput('smooth', 'Smoothing level:',
                                                       min = 0, max = 1, value = 0),
                                           uiOutput("startms")
                                           ),
                                    column(width = 9,
                                           dygraphOutput('msplot', width = "95%", height = '500px')))
                                )
                         )
                       )
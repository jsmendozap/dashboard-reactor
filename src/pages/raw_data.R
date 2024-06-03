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
                                           plotlyOutput('composition', height = '100vh'))
                                    )
                                  ),
                         tabPanel(title = "MS signals",
                                  fluidRow(
                                    column(width = 3,
                                           uiOutput('xms'),
                                           uiOutput('yms'),
                                           uiOutput('ms_events'),
                                           sliderInput('smooth', 'Smoothing level:',
                                                       min = 0, max = 1, value = 0),
                                           uiOutput("ms_int"),
                                           plotOutput('int_plot', height = '250px')
                                           ),
                                    tabBox(
                                      id = "tabset3", height = "100%", width = 9,
                                      tabPanel(title = "General", 
                                               fluidRow(
                                                column(width = 12,
                                                       dygraphOutput('msplot_gen', height = '450px'))
                                                )
                                               ),
                                      tabPanel(title = "Events", 
                                               fluidRow(
                                                 column(width = 12,
                                                        plotlyOutput('msplot', height = '100vh'))
                                               )
                                      )
                                      )
                                    )
                                )
                         )
                       )
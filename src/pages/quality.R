quality <- tabItem(tabName = 'quality',
               tabBox(
                 id = "tabset1", height = "100%", width = 12,
                 tabPanel(title = "Flow", 
                          fluidRow( 
                            tabBox(width = 7,
                              tabPanel(title = "Rate Flow", DTOutput('flow')),
                              tabPanel(title = "Correlations", DTOutput('corr'))
                              ),
                            box(title = "Normalized milimeters plot",
                                status = 'info',
                                solidHeader = T,
                                width = 5,
                                sliderInput('lag', label = 'Choose the number of points to derivate',
                                            min = 1, max = 50, value = 5),
                                plotlyOutput('norm')
                            )
                            )
                          ),
                 tabPanel(title = "Temperature",
                          fluidRow(
                            box(width = 7, status = 'info', solidHeader = T, DTOutput('temp')),
                            box(width = 5, status = 'info', solidHeader = T, plotlyOutput('plotTemp')))
                          ),
                 tabPanel(title = "Pressure",
                          fluidRow(
                            box(width = 7, status = 'info', solidHeader = T, DTOutput('press')),
                            box(width = 5, status = 'info', solidHeader = T,
                                dygraphOutput('plotPress'), title = 'Pressure difference per event plot')
                            )
                          )
               )
)
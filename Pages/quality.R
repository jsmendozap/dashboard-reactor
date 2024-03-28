quality <- tabItem(tabName = 'quality',
               tabBox(
                 id = "tabset1", height = "100%", width = 12,
                 tabPanel(title = "Flow", 
                          fluidRow( 
                            tabBox(width = 6,
                              tabPanel(title = "Total Flow", DTOutput('flow')),
                              tabPanel(title = "Correlations", DTOutput('corr'))
                              ),
                            box(title = "Normalized milimeters plot",
                                status = 'secondary',
                                solidHeader = T,
                                width = 6,
                                sliderInput('lag', label = 'Choose the number of points to derivate',
                                            min = 1, max = 50, value = 5),
                                plotlyOutput('norm')
                            )
                            )
                          ),
                 tabPanel("Temperature", "Tab content 2"),
                 tabPanel("Pressure", "Tab content 3")
               )
)
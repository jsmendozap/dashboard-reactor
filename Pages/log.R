log <- tabItem(tabName = 'log',
               fluidRow(
                 box(title = "Events summary",
                     status = 'primary',
                     solidHeader = T,
                     width = 7,
                     DTOutput('log')
                 ),
                 box(title = "Variable plot",
                     status = "info",
                     solidHeader = T,
                     width = 5,
                     selectInput(inputId = 'var', label = 'Select variable to print',
                                 choices = c("Air flow" = "fi_110",
                                             "CO2 flow" = "fi_120",
                                             "Argon/Propane flow" = "fi_130",
                                             "Nitrogen flow" = "fi_140",
                                             "Setted pressure" = "p_set",
                                             "Measured temp" = "tic_300_pv",
                                             "Setted temp" = "tic_300_sp")),
                     div(id = 'legend', style = "margin-left: 5px"),
                     dygraphOutput("dygraph", height = '55vh')
                 )
               )            
        )
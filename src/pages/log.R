choices <- c("Air flow" = "fi_110",
             "CO2 flow" = "fi_120",
             "Argon/Propane flow" = "fi_130",
             "Nitrogen flow" = "fi_140",
             "Setted pressure" = "p_set",
             "Measured temperature" = "tic_300_pv",
             "Setted temperature" = "tic_300_sp")

log <- tabItem(tabName = 'log',
               fluidRow(
                 column(
                   width = 8,
                   box(title = "Events summary",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       reactableOutput('log')
                       )),
                 column(
                   width = 4,
                   box(title = "Reactor's inflow",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       textOutput('valve'),
                       br(),
                       fluidRow(
                         column(
                           width = 6,
                           valueBoxOutput("fi_110", width = 12),
                           valueBoxOutput("fi_120", width = 12)),
                         column(
                           width = 6,
                           valueBoxOutput("fi_130", width = 12),
                           valueBoxOutput("fi_140", width = 12))
                       )
                   )
                 )
                 ),
               fluidRow(
                 column(
                   width = 6,
                   box(status = "info",
                       solidHeader = T,
                       width = 12,
                       selectInput(inputId = 'var', label = 'Select variable',
                                   choices = choices),
                       div(id = 'legend', style = "margin-left: 5px"),
                       br(),
                       dygraphOutput("dygraph", height = '45vh'))
                 ),
                 column(
                   width = 6,
                   box(status = "info",
                       solidHeader = T,
                       width = 12,
                       selectInput(inputId = 'var2', label = 'Select variable',
                                   choices = choices),
                       div(id = 'legend2', style = "margin-left: 5px"),
                       br(),
                       dygraphOutput("dygraph2", height = '45vh'))
                 )
               )
        )
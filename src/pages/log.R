choices <- c("Air flow" = "fi_110",
             "CO2 flow" = "fi_120",
             "Argon/Propane flow" = "fi_130",
             "Nitrogen flow" = "fi_140",
             "Setted pressure" = "p_set",
             "Measured temp" = "tic_300_pv",
             "Setted temp" = "tic_300_sp")

log <- tabItem(tabName = 'log',
               fluidRow(
                 column(
                   width = 8,
                   box(title = "Events summary",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       DTOutput('log'))),
                 column(
                   width = 4,
                   box(title = 'Filter events by duration', 
                       width = 12, solidHeader = T, status = 'info',
                       sliderInput(inputId = 'log_events',
                                   label = "Event's minimum duration (min)",
                                   min = 0, max = 60, value = 0)
                       ),
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
                       dygraphOutput("dygraph2", height = '45vh'))
                 )
               )
        )
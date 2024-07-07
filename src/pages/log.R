choices <- c("Air flow" = "fi_110",
             "CO2 flow" = "fi_120",
             "Argon/Propane flow" = "fi_130",
             "Nitrogen flow" = "fi_140",
             "Setted pressure" = "p_set",
             "Measured temperature" = "tic_300_pv",
             "Setted temperature" = "tic_300_sp")

log_ui <- function(id){

  ns <- NS(id)

  tabItem(tabName = 'log',
               fluidRow(
                 column(
                   width = 8,
                   box(title = "Events summary",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       reactableOutput(ns('log'))
                       )),
                 column(
                   width = 4,
                   box(title = "Leak test", width = 12,
                       status = 'info', solidHeader = T,
                       uiOutput(ns('leak'))),
                   box(title = "Reactor's inflow",
                       status = 'info',
                       solidHeader = T,
                       width = 12,
                       textOutput(ns('valve')),
                       br(),
                       fluidRow(
                         column(
                           width = 6,
                           valueBoxOutput(ns("fi_110"), width = 12),
                           valueBoxOutput(ns("fi_120"), width = 12)),
                         column(
                           width = 6,
                           valueBoxOutput(ns("fi_130"), width = 12),
                           valueBoxOutput(ns("fi_140"), width = 12))
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
                       selectInput(inputId = ns('var'), label = 'Select variable',
                                   choices = choices),
                       div(id = 'legend', style = "margin-left: 5px"),
                       br(),
                       dygraphOutput(ns("dygraph"), height = '45vh'))
                 ),
                 column(
                   width = 6,
                   box(status = "info",
                       solidHeader = T,
                       width = 12,
                       selectInput(inputId = ns('var2'), label = 'Select variable',
                                   choices = choices),
                       div(id = 'legend2', style = "margin-left: 5px"),
                       br(),
                       dygraphOutput(ns("dygraph2"), height = '45vh'))
                 )
               )
        )
}
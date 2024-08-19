raw_data_ui <- function(id) {

       ns <- shiny::NS(id)

       bs4Dash::tabItem(tabName = "raw-data",
              bs4Dash::tabBox(
              id = "tabset2", height = "100%", width = 12,
              shiny::tabPanel(title = "GC composition",
                     shiny::fluidRow(
                            bs4Dash::column(width = 3,
                                   shiny::selectInput(ns('gc_type'), "GC plot type", c("General", "Events")),
                                   shiny::uiOutput(ns('UIcompunds')),
                                   shiny::uiOutput(ns('gc_events')),
                                   shiny::uiOutput(ns('xgc'))),
                            bs4Dash::column(width = 9,
                                   plotly::plotlyOutput(ns('composition'), height = '100%'))
                            )
                     ),
              shiny::tabPanel(title = "MS signals",
                     shiny::fluidRow(
                            bs4Dash::column(width = 3,
                                   shiny::uiOutput(ns('xms')),
                                   shiny::uiOutput(ns('yms')),
                                   shiny::uiOutput(ns('ms_events')),
                                   shiny::checkboxInput(ns('scale'), 'Same scale', TRUE),
                                   shiny::sliderInput(ns('smooth'), 'Smoothing level:',
                                          min = 0, max = 1, value = 0),
                                   shiny::uiOutput(ns("ms_int")),
                                   shiny::plotOutput(ns('int_plot'), height = '250px')
                                   ),
                            bs4Dash::tabBox(
                            id = "tabset3", height = "80%", width = 9,
                            shiny::tabPanel(title = "General",
                                   shiny::fluidRow(
                                   bs4Dash::column(width = 12,
                                          dygraphs::dygraphOutput(ns('msplot_gen'), height = '380px'))
                                   )
                                   ),
                            shiny::tabPanel(title = "Events",
                                   shiny::fluidRow(
                                   bs4Dash::column(width = 12,
                                          plotly::plotlyOutput(ns('msplot'), height = '100%'))
                                   )
                            )
                            )
                            )
                     )
              )
              )
}

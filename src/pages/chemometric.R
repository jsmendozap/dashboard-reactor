chemometric_ui <- function(id) {

       ns <- shiny::NS(id)

       bs4Dash::tabItem(tabName = "chemometric",
                       shiny::fluidRow(reactable::reactableOutput(ns('std'), width = '100%')),
                       shiny::br(),
                       shiny::fluidRow(
                         bs4Dash::column(width = 3,
                                shiny::uiOutput(ns('flow_compounds')),
                                shiny::uiOutput(ns('flow_events')),
                                bs4Dash::actionButton(ns('btn_flow'), label = 'Calculate flows')),
                         bs4Dash::tabBox(width = 9,
                                shiny::tabPanel(title = "Molar rate",
                                         shiny::fluidRow(
                                           bs4Dash::column(width = 12,
                                                 plotly::plotlyOutput(ns('molar_flow'), height = '50%'))
                                          )
                                   ),
                                shiny::tabPanel(title = "Conversion",
                                         shiny::fluidRow(
                                           bs4Dash::column(width = 12,
                                                 plotly::plotlyOutput(ns('conversion'), height = '30%'))
                                          )
                                   ),
                                shiny::tabPanel(title = "Mass balance",
                                          shiny::fluidRow(
                                           bs4Dash::column(width = 6, plotly::plotlyOutput(ns('mass_balance'), height = '30%')),
                                           bs4Dash::column(width = 6, reactable::reactableOutput(ns('mass_summary')))
                                          )
                                   ),         
                                shiny::tabPanel(title = "Box plot",
                                          shiny::fluidRow(
                                          bs4Dash::column(width = 12,
                                                 plotly::plotlyOutput(ns('boxplot'), height = '30%'))
                                          )
                                   )
                          )
                       )
       )

}

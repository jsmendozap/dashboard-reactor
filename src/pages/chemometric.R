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
                                shiny::tabPanel(title = "Molar plot",
                                         shiny::fluidRow(
                                           bs4Dash::column(width = 12,
                                                 plotly::plotlyOutput(ns('molar_flow'), height = '50%'))
                                          )),
                                shiny::tabPanel(title = "Box plot",
                                         shiny::fluidRow(
                                           bs4Dash::column(width = 12,
                                                  plotly::plotlyOutput(ns('boxplot'), height = '30%'))
                                         ))
                          )
                       )
)

}

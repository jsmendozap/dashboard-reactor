chemometric_ui <- function(id) {

       ns <- NS(id)

       tabItem(tabName = "chemometric",
                       fluidRow(reactableOutput(ns('std'), width = '100%')),
                       br(),
                       fluidRow(
                         column(width = 3,
                                uiOutput(ns('flow_compounds')),
                                uiOutput(ns('flow_events')),
                                actionButton(ns('btn_flow'), label = 'Calculate flows')),
                         tabBox(width = 9,
                                tabPanel(title = "Molar plot",
                                         fluidRow(
                                           column(width = 12, 
                                                 plotlyOutput(ns('molar_flow'), height = '50%'))
                                          )),
                                tabPanel(title = "Box plot",
                                         fluidRow(
                                           column(width = 12, 
                                                  plotOutput(ns('boxplot'), height = '50vh'))
                                         ))
                          )
                       )
)

}
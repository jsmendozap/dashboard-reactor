chemometric <- tabItem(tabName = "chemometric",
                       fluidRow(reactableOutput('std', width = '100%'),
                                actionButton('btn_flow', label = 'Calculate')),
                       br(),
                       fluidRow(
                         column(width = 3,
                                uiOutput('flow_compounds'),
                                uiOutput('flow_events')),
                         tabBox(width = 9,
                                tabPanel(title = "Molar plot",
                                         fluidRow(
                                           column(width = 12, 
                                                  plotlyOutput('molar_flow', height = '50vh'))
                                         )),
                                tabPanel(title = "Box plot",
                                         fluidRow(
                                           column(width = 12, 
                                                  plotOutput('boxplot', height = '50vh'))
                                         ))
                          )
                       )
)

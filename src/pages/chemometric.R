chemometric <- tabItem(tabName = "chemometric",
                       fluidRow(reactableOutput('std', width = '100%')),
                       br(),
                       fluidRow(
                         column(width = 3,
                                uiOutput('flow_compounds'),
                                uiOutput('flow_events'),
                                actionButton('btn_flow', label = 'Calculate flows')),
                         tabBox(width = 9,
                                tabPanel(title = "Molar plot",
                                         fluidRow(
                                           column(width = 12, 
                                                  plotlyOutput('molar_flow', height = '400px'))
                                         )),
                                tabPanel(title = "Box plot",
                                         fluidRow(
                                           column(width = 12, 
                                                  plotOutput('boxplot', height = '50vh'))
                                         ))
                          )
                       )
)

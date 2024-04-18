composition <- tabItem(tabName = 'composition',
                       tabBox(
                         id = "tabset2", height = "100%", width = 12,
                         tabPanel(title = "Composition",
                                  uiOutput('UIcompunds'),
                                  fluidRow(plotlyOutput('composition', height = '110vh'))
                                  ),
                         tabPanel(title = "Volumetric flow"),
                         tabPanel(title = "Mass flow")
                         )
                       )
chemometric <- tabItem(tabName = "chemometric",
                        fluidRow(
                          reactableOutput('std', width = '100%'),
                          actionButton('btn_flow', label = 'Calculate')
                        )
  
)
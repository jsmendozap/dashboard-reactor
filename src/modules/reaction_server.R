reaction_server <- function(id) {
  moduleServer(id, function(input, output, session){
  
  output$descrip <- renderUI({
      req(input$formula)

      
    })
  })
}
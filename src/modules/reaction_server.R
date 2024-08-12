reaction_server <- function(id) {
  moduleServer(id, function(input, output, session){
  
    ns <- NS(id)
    table <- reactiveVal(
      tibble(
        ID = numeric(),
        Compound = character(),
        Type = character(),
        MFC_1 = numeric(),
        MFC_2 = numeric(),
        MFC_3 = numeric(),
        MFC_4 = numeric()
      )
    )
    
    observeEvent(input$new, {
      table() %>%
        add_row(
          ID = 1,
          Compound = input$compound,
          Type = input$c_type,
          MFC_1 = input$mfc1,
          MFC_2 = input$mfc2,
          MFC_3 = input$mfc3,
          MFC_4 = input$mfc4,
        ) %>%
        mutate(ID = row_number()) %>%
        table()
    })
    
    output$compounds_table <- renderReactable({
      table() %>%
        rename_with(.fn = ~str_replace_all(.x, '_', ' '), .cols = contains('MFC')) %>%
        custom_reactable(columns = list(Compound = colDef(minWidth = 250)),
                         style = "border-radius: 3px"
                        )
    })
    
  })
}
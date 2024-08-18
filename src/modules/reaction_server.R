reaction_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- shiny::NS(id)
    table <- shiny::reactiveVal(
      dplyr::tibble(
        ID = numeric(),
        Compound = character(),
        Type = character(),
        CMP_1 = numeric(),
        CMP_2 = numeric(),
        CMP_3 = numeric(),
        CMP_4 = numeric()
      )
    )

    shiny::observeEvent(input$new, {
      table() %>%
        dplyr::add_row(
          ID = 1,
          Compound = input$compound,
          Type = input$c_type,
          CMP_1 = input$mfc1,
          CMP_2 = input$mfc2,
          CMP_3 = input$mfc3,
          CMP_4 = input$mfc4,
        ) %>%
        dplyr::mutate(ID = dplyr::row_number()) %>%
        table()

      purrr::map(.x = c("mfc1", "mfc2", "mfc3", "mfc4"),
                 .f = ~shiny::updateNumericInput(session, .x, value = NA))
      purrr::map(.x = c("compound", "c_type"),
                .f = ~shiny::updateTextInput(session, .x, value = ""))
    })

    output$compounds_table <- reactable::renderReactable({
      table() %>%
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('CMP')) %>%
        custom_reactable(columns = list(Compound = reactable::colDef(minWidth = 250)),
                         style = "border-radius: 3px", selection = 'single'
                        )
    })
    
    shiny::observeEvent(input$save, {
      if (nchar(input$save) > 0){
        query("insert", "reactions", data.frame(name = input$save))
        query("insert", "settings", table() %>% dplyr::mutate(reaction = input$save))
        
        shinyWidgets::updateSearchInput(session = session, inputId = "save", value = "")
        
        table(dplyr::tibble(
          ID = numeric(),
          Compound = character(),
          Type = character(),
          CMP_1 = numeric(),
          CMP_2 = numeric(),
          CMP_3 = numeric(),
          CMP_4 = numeric()
        ))
        
        shinyWidgets::updatePickerInput(session = session, 
          inputId = "reactions_db", 
          choices = query("list", "reactions", "name"))
        
        shinyToastify::showToast(
          session,
          input,
          text = "Reaction saved sussessfully",
          type = "info",
          autoClose = 3000,
          position = "bottom-right"
        )
      }
      })
    
    observeEvent(input$remove, {
      sel <- reactable::getReactableState('compounds_table', 'selected')
      
      table() %>%
        dplyr::filter(ID != sel) %>%
        dplyr::mutate(ID = row_number()) %>%
        table()

      table() %>%
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('MFC')) %>%
        {reactable::updateReactable('compounds_table', .)}
      
    })
      
    ### Load reactions  
    
    react_table <- shiny::reactiveVal(F)
    observe({
      react_table(query("exist", "reactions"))
    })

    output$reactions_table <- shiny::renderUI({
      if(react_table()) {
        shinyWidgets::pickerInput(inputId = ns('reactions_db'),
                                  label = 'Reactions',
                                  choices = query("list", "reactions", "name"))
      }
    })

    output$setting <- reactable::renderReactable({
      req(input$reactions_db)
      
      query("head", "settings", input$reactions_db) %>%
        dplyr::select(-c(ID, reaction)) %>%
          dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '),
                             .cols = dplyr::contains('MFC')) %>%
        custom_reactable()
    })

    return(reactive({ input$reactions_db }))
  })
}

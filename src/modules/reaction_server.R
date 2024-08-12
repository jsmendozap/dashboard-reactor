reaction_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- shiny::NS(id)
    table <- shiny::reactiveVal(
      dplyr::tibble(
        ID = numeric(),
        Compound = character(),
        Type = character(),
        MFC_1 = numeric(),
        MFC_2 = numeric(),
        MFC_3 = numeric(),
        MFC_4 = numeric()
      )
    )

    shiny::observeEvent(input$new, {
      table() %>%
        dplyr::add_row(
          ID = 1,
          Compound = input$compound,
          Type = input$c_type,
          MFC_1 = input$mfc1,
          MFC_2 = input$mfc2,
          MFC_3 = input$mfc3,
          MFC_4 = input$mfc4,
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
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('MFC')) %>%
        custom_reactable(columns = list(Compound = reactable::colDef(minWidth = 250)),
                         style = "border-radius: 3px"
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
          MFC_1 = numeric(),
          MFC_2 = numeric(),
          MFC_3 = numeric(),
          MFC_4 = numeric()
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
  })
}

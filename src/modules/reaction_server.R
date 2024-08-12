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
      shiny::updateTextInput(session, "compound", value = "")
    })

    output$compounds_table <- reactable::renderReactable({
      table() %>%
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('MFC')) %>%
        custom_reactable(columns = list(Compound = reactable::colDef(minWidth = 250)),
                         style = "border-radius: 3px"
                        )
    })

  })
}

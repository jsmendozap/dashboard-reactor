reaction_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    output$reactions_table <- shiny::renderUI({
      shinyWidgets::multiInput(
        inputId = ns('reactions_db'),
        label = "Reactions",
        choices = list.files(path = "~/Reactor Dashboard/Reactions") %>%
          purrr::map(.f = ~ tools::file_path_sans_ext(.x)),
        width = "100%",
        options = list(limit = 1)
      )
    })

    setting <- shiny::reactive({
      req(input$reactions_db)

      res <- openxlsx::readWorkbook(
        fs::path(
          "~/Reactor Dashboard/Reactions",
          paste0(input$reactions_db, ".", "xlsx")
        )
      )

      list(
        desc = res[1, 3],
        data = res %>%
          dplyr::filter(dplyr::row_number() > 7) %>%
          dplyr::select(2:16) %>%
          setNames(res[7, 2:16]) %>%
          dplyr::rename("Compound" = 1) %>%
          tidyr::drop_na(Compound)
      )
    })

    output$reaction_desc <- shiny::renderUI({
      shiny::tagList(
        shiny::strong("Description"),
        shiny::br(),
        shiny::p(setting()[[1]])
      )
    })

    output$setting <- reactable::renderReactable({
      req(input$reactions_db)

      setting()[[2]][, c(1, 8:14)] %>%
        custom_reactable(
          columns = list(
            Compound = reactable::colDef(sticky = "left", width = 150)
          )
        )
    })

    return(shiny::reactive({
      setting()[[2]] %>%
        janitor::clean_names()
    }))
  })
}

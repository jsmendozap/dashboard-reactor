log_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- shiny::NS(id)

    selected1 <- shiny::reactive({ input$var })
    selected2 <- shiny::reactive({ input$var2 })

    output$dygraph <- dygraphs::renderDygraph({
      shiny::req(app_state$bd)
      renderdy(app_state$bd(), selected1(), input, 'legend')
    })

    output$dygraph2 <- dygraphs::renderDygraph({
      shiny::req(app_state$bd)
      renderdy(app_state$bd(), selected2(), input, 'legend2')
    })

    output$log <- reactable::renderReactable({
      shiny::req(app_state$bd)

      app_state$bd() %>%
        dplyr::filter(name == mode(name), .by = event) %>%
        dplyr::slice_head(n = 1, by = event) %>%
        dplyr::select(event, date_time, name, time_duration) %>%
        dplyr::mutate(obs = "") %>%
        custom_reactable(
          columns = list(
            event = reactable::colDef(name = 'Event', width = 80),
            date_time = reactable::colDef(name = 'Start time', width = 200),
            name = reactable::colDef(name = 'Event name', minWidth = 100),
            time_duration = reactable::colDef(name = 'Duration', width = 100),
            obs = reactable::colDef(name = 'Observations', width = 150,
                                    cell = reactable.extras::text_extra(ns("comment"), class = 'obs'))
          ),
          selection = 'single'
        )
    })

    obs <- shiny::reactiveValues()

    shiny::observeEvent(input$comment, {
      row <- unique(app_state$bd()$event)[input$comment$row] %>% as.character()
      obs[[row]] <- input$comment$value
      app_state$comment <- obs
    })

    output$leak <- shiny::renderUI({
      shiny::req(app_state$df)

      test <- app_state$df() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sum = sum(fic_110, fic_120, fic_130, fic_140)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n > 5 & p_set != 0 & sum == 0) %>%
        dplyr::select(event, n, p_set, sum, pt_310) %>%
        dplyr::filter(event == max(event)) %>%
        dplyr::slice(c(1, n())) %>%
        {c(event = unique(.$event), dif = .$pt_310[1] - .$pt_310[2])}

      shiny::renderPrint({
        if(!is.na(test[1])){
          stringr::str_glue("Event: {test[1]} \nPressure difference: {round(test[2], 2)}")
        } else {
          cat("No Leak test found in data")
        }
       })
    })

    output$valve <- shiny::renderText({
      shiny::req(reactable::getReactableState('log', 'selected'))

      sel <- reactable::getReactableState('log', 'selected')
      pos <- app_state$bd() %>%
        dplyr::slice_head(n = 1, by = event) %>%
        dplyr::pull(rswitch_val)

      paste('Valve position:', pos[sel])
    })

    output$fi_110 <- bs4Dash::renderValueBox({
      reactor_values(app_state$bd(), 'fi_110', 'Air')
    })

    output$fi_120 <- bs4Dash::renderValueBox({
      reactor_values(app_state$bd(), 'fi_120', 'Carbon dioxide')
    })

    output$fi_130 <- bs4Dash::renderValueBox({
      reactor_values(app_state$bd(), 'fi_130', 'Argon / Propane')
    })

    output$fi_140 <- bs4Dash::renderValueBox({
      reactor_values(app_state$bd(), 'fi_140', 'Nitrogen')
    })

    return(shiny::reactive({
      
        shiny::reactiveValuesToList(obs) %>% 
        {
          if(length(.) > 0) {
            utils::stack(.) %>%
            dplyr::rename(comment = values, event = ind) %>%
            dplyr::mutate(event = as.double(as.character(event)))
          }
        }
      })
    )

    })
}


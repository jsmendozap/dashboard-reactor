chem_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    output$std <- reactable::renderReactable({
      shiny::req(app_state$bd)

      tecq <- c('None', 'CO2-O2/TPO', 'Regeneration', 'CO2-TPD', 'Propane-TPD', 'H2-TPR', 'Propane-TPR', 'By Pass', 'TOS', 'Ramp')
      is <- c('None', 'Nitrogen', 'Argon')

      app_state$bd() %>%
        dplyr::slice_head(n = 1, by = event) %>%
        plotly::select(event, name) %>%
        dplyr::mutate(technique = list(tecq), is = list(is), qis = NA) %>%
        custom_reactable(columns = list(
                           event = reactable::colDef(name = 'Event'),
                           name = reactable::colDef(name = 'Event name', minWidth = 300),
                           technique = reactable::colDef(name = 'Technique/Reaction', minWidth = 150,
                                              cell = reactable.extras::dropdown_extra(id = ns('dropdown'),
                                                                choices = tecq,
                                                                class = 'dropdown-extra')),
                           is = reactable::colDef(name = 'Internal Standard', minWidth = 150,
                                       cell = reactable.extras::dropdown_extra(id = ns('is'), choices = is,
                                                             class = 'dropdown-extra')),
                           qis = reactable::colDef(name = 'QIS (NmL/min)', minWidth = 100,
                                        cell = reactable.extras::text_extra(ns('qis'), class = 'text-input'))
                         ), style = "border-radius: '3px'"
                        )
    })

    tech <- shiny::reactiveValues()
    is <- shiny::reactiveValues()
    qis <- shiny::reactiveValues()

    shiny::observeEvent(input$dropdown, {
      name <- as.character(input$dropdown$row)
      tech[[name]] <- input$dropdown$value
    })

    shiny::observeEvent(input$is, {
      name <- as.character(input$is$row)
      is[[name]] <- input$is$value
    })

    shiny::observeEvent(input$qis, {
      name <- as.character(input$qis$row)
      qis[[name]] <- input$qis$value
    })

    mass <- shiny::reactive({
      app_state$path() %>% as.character %>% strsplit(" ") %>%
      unlist %>% {.[length(.) - 1]} %>% as.numeric(.)/1000000
    })

    chem_values <- shiny::eventReactive(input$btn_flow, {

      sel_events <- function(x, qis) {
        events <- x %>% unique
        filter <- shiny::reactiveValuesToList(qis) %>% unlist %>% purrr::discard(\(x) nchar(x) == 0) %>% names %>% as.numeric
        events[filter]
      }

      data.frame(
        event = sel_events(app_state$bd()$event, qis),
        name = sel_events(app_state$bd()$name, qis),
        technique = shiny::reactiveValuesToList(tech) %>% unlist %>% purrr::discard(~.x == "None"),
        is = shiny::reactiveValuesToList(is) %>% unlist %>% purrr::discard(~.x == "None") %>% tolower,
        qis = shiny::reactiveValuesToList(qis) %>% unlist %>% as.numeric %>% purrr::discard(is.na)) %>%
        dplyr::left_join(app_state$gc(), by = dplyr::join_by('event')) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(10:ncol(.), ~ qis * (. / get(is)) * (60 / (22.4 * mass() * 1000)))) %>%
        dplyr::ungroup()
    })

    ## Molar flow --------------------------------------------------------------------------------------------------------

    output$flow_compounds <- shiny::renderUI({
      shiny::req(app_state$gc)

      comp <- colnames(app_state$gc())[-c(1:5)] %>%
        {.[which(!. %in% c('nitrogen', 'argon'))]} %>%
        stringr::str_replace_all('_', ' ') %>%
        stringr::str_to_title()

      shinyWidgets::pickerInput(
        inputId = ns("graph_compounds"), label = "Select compounds to plot",
        choices = comp, multiple = TRUE, selected = comp,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
      )
    })

    output$flow_events <- shiny::renderUI({

      selected <- chem_values() %>% dplyr::pull(event) %>% unique()

      shinyWidgets::pickerInput(
        inputId = ns("graph_event"), label = "Select events to plot",
        choices = selected, multiple = TRUE, selected = selected,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
      })

    output$molar_flow <- plotly::renderPlotly({
      shiny::req(chem_values())
      shiny::req(input$graph_compounds)
      shiny::req(input$graph_event)

      event_names <- chem_values() %>%
        dplyr::summarise(name = unique(name), .by = event) %>%
        dplyr::mutate(name = stringr::str_c("Event: ", event, " - ", name)) %>%
        {setNames(.$name, .$event)}

      plot <- chem_values() %>%
        dplyr::select(event, time, 9:ncol(.)) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::filter(!Compound %in% c('argon', 'nitrogen')) %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ') %>% stringr::str_to_title()) %>%
        plotly::filter(Compound %in% input$graph_compounds & event %in% input$graph_event) %>%
        ggplot2::ggplot(ggplot2::aes(x = time, y = value, fill = Compound)) +
        ggplot2::geom_area(alpha = 0.6, color = 'black', linewidth = 0.2) +
        ggplot2::labs(x = 'Time (min)', y = "Molar flow (mol/h)") +
        ggplot2::facet_wrap(~event, scales = 'fixed', ncol = 2,
                            labeller = ggplot2::as_labeller(event_names)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
                       axis.title = ggplot2::element_text(size = 12),
                       panel.spacing = ggplot2::unit(0.5, "cm"),
                       plot.margin = ggplot2::unit(c(0, 0, 2, 2), 'cm'),
                       panel.background = ggplot2::element_rect(colour = 'black'))

      total_height <- 180 * length(unique(chem_values()$event))
      plotly::ggplotly(plot, height = total_height, dynamicTicks = T, tooltip = "fill")
    })

    ### Conversion --------------------------------------------------------------------------------------------
    
    reactants <- shiny::reactive({
      query("head", "settings", app_state$setting()) %>%
        dplyr::filter(Type == "Reactant") %>%
        dplyr::pull(Compound) %>%
        stringr::str_replace_all(" ", "_") %>%
        tolower()
    })
    
    avgs <- shiny::reactive({
      plyr::ddply(.data = chem_values() %>% dplyr::filter(technique == 'By Pass'),
                  .variables =  'event',
                  .fun = \(x) {
                    mean_flow <- \(x) x[which(!x %in% boxplot.stats(x)$out)] %>% mean
                    
                    x %>%
                      dplyr::select(technique, dplyr::all_of(reactants())) %>%
                      dplyr::summarise(technique = unique(technique),
                                       dplyr::across(.cols = 2:ncol(.), .fns = ~mean_flow(.x)))
                  })
    })

    bypass <- shiny::reactive({
      chem_values() %>%
      dplyr::select(event, technique) %>%
      dplyr::distinct() %>%
      dplyr::filter(technique == 'By Pass') %>%
      dplyr::left_join(avgs()) %>%
      dplyr::transmute(event, technique,
                       dplyr::across(.cols = reactants(), .names = "{.col}_bypass")
      )
    })
    
    event_names <- shiny::reactive({
      chem_values() %>%
        dplyr::filter(technique == 'TOS') %>%
        dplyr::summarise(name = unique(name), .by = event) %>%
        dplyr::mutate(name = stringr::str_c("Event: ", event, " - ", name)) %>%
        {setNames(.$name, .$event)} 
    })

    output$conversion <- plotly::renderPlotly({
      req(app_state$setting())

      conversion_plot <- chem_values() %>%
        dplyr::filter(technique == 'TOS') %>%
        dplyr::left_join(bypass() %>% dplyr::mutate(event = event + 1), by = dplyr::join_by('event')) %>%
        dplyr::mutate(dplyr::across(.cols = reactants(), 
                                    .fns = ~ 100 * ((get(paste0(dplyr::cur_column(), "_bypass")) - .)/get(paste0(dplyr::cur_column(), "_bypass"))))) %>%
        dplyr::select(time, event, dplyr::all_of(reactants())) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ') %>% stringr::str_to_title()) %>%
        ggplot2::ggplot(aes(x = time, y = value, fill = Compound, shape = Compound)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::labs(x = 'Time (min)', y = "Conversion (%)") +
        ggplot2::scale_y_continuous(n.breaks = 10) +
        ggplot2::facet_wrap(~event, ncol = 2, labeller = ggplot2::as_labeller(event_names())) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(color = 'black', size = 10),
                       axis.title.y = ggplot2::element_text(size = 12),
                       panel.background = ggplot2::element_rect(colour = 'black'))
                       
      total_height <- 180 + 180 * length(input$graph_event)/2
      plotly::ggplotly(conversion_plot, height = total_height)
    })

    ### Mass balance -------------------------------------------------------------------------------------------

    output$mass_balance <- plotly::renderPlotly({

      c_in <- compounds %>%
        dplyr::mutate(name = stringr::str_replace_all(name, " ", "_") %>% tolower()) %>%
        dplyr::filter(name %in% names(chem_values()))
      
      mass_plot <- chem_values() %>%
          dplyr::filter(technique == 'TOS') %>%
          dplyr::select(event, time, dplyr::any_of(c_in$name))
        
      b_in <- bypass() %>% 
            dplyr::relocate(event, technique, dplyr::contains(c_in$name)) %>%
            {as.matrix(x = .[,3:ncol(.)])} %*% as.matrix(dplyr::filter(c_in, name %in% reactants()) %>% .[,4:6]) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(event = bypass()$event + 1)

      mass_plot <- as.matrix(mass_plot[,3:ncol(mass_plot)]) %*% as.matrix(c_in[,4:6]) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(event = mass_plot$event, time = mass_plot$time) %>%
        dplyr::left_join(b_in, by = dplyr::join_by('event'), suffix = c("_out", "_in")) %>%
        dplyr::transmute(event, time, 
                          across(.cols = ends_with("_out"),
                                .fns =  ~ .x / get(stringr::str_replace(dplyr::cur_column(), "_out", "_in")), 
                                .names = "{stringr::str_replace(.col, '_out', '')}")) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = "Compound", values_to = "value") %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ') %>% stringr::str_to_title()) %>%
        ggplot2::ggplot(aes(x = time, y = value, fill = Compound)) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::labs(x = 'Time (min)', y = "Mass balance") +
            ggplot2::scale_y_continuous(n.breaks = 10) +
            ggplot2::facet_wrap(~event, ncol = 2, labeller = ggplot2::as_labeller(event_names())) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.y = ggplot2::element_text(color = 'black', size = 10),
                           axis.title.y = ggplot2::element_text(size = 12),
                           panel.background = ggplot2::element_rect(colour = 'black'))
                           
        total_height <- 180 + 180 * length(input$graph_event)/2
        plotly::ggplotly(mass_plot, height = total_height)

    })

    ### Boxplot ------------------------------------------------------------------------------------------------
    
    #dplyr::filter(Compound %in% input$graph_compounds & event %in% input$graph_event) %>%
    
    #ggplot2::geom_boxplot() +
    #ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, fill = "gray", size = 2) +
    #ggplot2::stat_summary(fun = min, geom = "point", shape = 25) +
    #ggplot2::stat_summary(fun = max, geom = "point", shape = 19) +
    
  })
}

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
      
      sel_events <- function(which) {
        events <- app_state$bd() %>%
                    dplyr::distinct(event, name) %>%
                    dplyr::pull({{ which }})
        filter <- shiny::reactiveValuesToList(qis) %>% unlist %>% purrr::discard(\(x) nchar(x) == 0) %>% names %>% as.numeric
        events[filter]
      }

      data.frame(
        event = sel_events("event"),
        name = sel_events("name"),
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
        change_str('_', ' ')

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

    mf <- reactive({
      shiny::req(chem_values())
      shiny::req(input$graph_compounds)
      shiny::req(input$graph_event)
      
      chem_values() %>%
        dplyr::select(event, time, 9:ncol(.)) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::filter(!Compound %in% c('argon', 'nitrogen')) %>%
        dplyr::mutate(Compound = change_str(Compound, '_', ' ')) %>%
        dplyr::filter(Compound %in% input$graph_compounds & event %in% input$graph_event)
    })
    
    output$molar_flow <- plotly::renderPlotly({
      shiny::req(chem_values())

      event_names <- chem_values() %>%
        dplyr::summarise(name = unique(name), .by = event) %>% 
        dplyr::mutate(name = stringr::str_c("Event: ", event, " - ", name)) %>%
        {setNames(.$name, .$event)}

      molar_plot <- mf() %>%
        plot(x = time,
             y = value, 
             fill = Compound, 
             area = T,
             xlab = 'Time (min)',
             ylab = 'Molar flow (mol/h)',
             facet = "event",
             custom_color = T,
             args = list(
              area = list(alpha = 0.6, color = 'black', linewidth = 0.2),
              facet = list(scales = 'fixed', ncol = 2, labeller = ggplot2::as_labeller(event_names))
             )
        ) +
        ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
                       axis.title = ggplot2::element_text(size = 12),
                       panel.spacing = ggplot2::unit(0.5, "cm"),
                       plot.margin = ggplot2::unit(c(0, 0, 2, 2), 'cm'),
                       panel.background = ggplot2::element_rect(colour = 'black'))
        
      total_height <- 180 * length(unique(chem_values()$event))
      plotly::ggplotly(molar_plot, height = total_height, dynamicTicks = T, tooltip = "fill")
    })

    ### Conversion --------------------------------------------------------------------------------------------
    
    reactants <- shiny::reactive({
      query("head", "settings", app_state$setting()) %>%
        dplyr::filter(Type == "Reactant") %>%
        dplyr::pull(Compound) %>%
        change_str(" ", "_", T)
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

    conversion <- shiny::reactive({
      req(app_state$setting())
      
      chem_values() %>%
        dplyr::filter(technique == 'TOS') %>%
        dplyr::left_join(bypass() %>% dplyr::mutate(event = event + 1), by = dplyr::join_by('event')) %>%
        dplyr::mutate(dplyr::across(.cols = reactants(), 
                                    .fns = ~ 100 * ((get(paste0(dplyr::cur_column(), "_bypass")) - .)/get(paste0(dplyr::cur_column(), "_bypass"))))) %>%
        dplyr::select(time, event, dplyr::all_of(reactants())) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(Compound = change_str(Compound, '_', ' '))
    })

    output$conversion <- plotly::renderPlotly({
      req(conversion())

      conversion_plot <-  conversion() %>%
        plot(x = time, 
             y = value, 
             fill = Compound,
             shape = Compound,
             lines = T,
             points = T,
             xlab = 'Time (min)',
             ylab = 'Conversion (%)',
             facet = 'event',
             scale_y = list(n.breaks = 10),
             args = list(facet = list(ncol = 2, labeller = ggplot2::as_labeller(event_names())))
        ) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(color = 'black', size = 10),
                       axis.title.y = ggplot2::element_text(size = 12),
                       panel.spacing = ggplot2::unit(0.5, "cm"),
                       plot.margin = ggplot2::unit(c(0, 0, 2, 2), 'cm'),
                       panel.background = ggplot2::element_rect(colour = 'black'))
                       
      total_height <- 180 + 180 * length(input$graph_event)/2
      plotly::ggplotly(conversion_plot, height = total_height, dynamicTicks = T)
    })

    ### Mass balance -------------------------------------------------------------------------------------------

    mb <- shiny::reactive({

      c_in <- compounds %>%
        dplyr::mutate(name = change_str(name, " ", "_", T)) %>%
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
        dplyr::mutate(Compound = change_str(Compound, '_', ' '), value = value * 100) %>%
        dplyr::filter(event %in% input$graph_event)
      })

    output$mass_balance <- plotly::renderPlotly({
      req(mb())

      mass_plot <- mb() %>%
        plot(x = time,
             y = value, 
             fill = Compound,
             lines = T,
             points = T,
             xlab = "Time (min)",
             ylab = "Mass balance",
             facet = 'event',
             scale_y = list(n.breaks = 10),
             args = list(facet = list(ncol = 1, labeller = ggplot2::as_labeller(event_names())))
        ) + 
          ggplot2::theme(axis.text.y = ggplot2::element_text(color = 'black', size = 10),
                         axis.title.y = ggplot2::element_text(size = 12),
                         panel.spacing = ggplot2::unit(0.5, "cm"),
                         plot.margin = ggplot2::unit(c(0, 0, 2, 2), 'cm'),
                         panel.background = ggplot2::element_rect(colour = 'black'))
        
        total_height <- 180 + 180 * length(input$graph_event)/2
        plotly::ggplotly(mass_plot, height = total_height, dynamicTicks = T)

    })

    output$mass_summary <- reactable::renderReactable({

      mb() %>%
        dplyr::group_by(event, Compound) %>%
        dplyr::summarise(avg = round(mean(value), 2), sd = round(sd(value), 2)) %>% 
        dplyr::filter(event %in% input$graph_event) %>%
        custom_reactable(
          columns = list(
            event = reactable::colDef(name = 'Event'),
            Compound = reactable::colDef(name = 'Compound'),
            avg = reactable::colDef(name = 'Average'),
            sd = reactable::colDef(name = 'Standard deviation')
          ), style = "border-radius: '3px'; margin-bottom: 10px"
        )
    })

    ### Boxplot ------------------------------------------------------------------------------------------------
    
    boxplot_data <- shiny::reactive({
      chem_values() %>%
        dplyr::filter(technique == 'TOS') %>%
        dplyr::left_join(bypass() %>% dplyr::mutate(event = event + 1), by = dplyr::join_by('event')) %>%
        dplyr::mutate(dplyr::across(.cols = reactants(), .fns = ~(get(paste0(dplyr::cur_column(), "_bypass")) - .))) %>%
        dplyr::select(time, event, 10:20) %>%
        tidyr::drop_na(time) %>%
        tidyr::pivot_longer(cols = 3:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(Compound = change_str(Compound, '_', ' ')) %>%
        dplyr::filter(Compound %in% input$graph_compounds & event %in% input$graph_event)
    })

    output$boxplot <- plotly::renderPlotly({

      p <- boxplot_data() %>%
            plot(x = Compound,
                 y = value, 
                 fill = Compound,
                 boxp = T,
                 xlab = "",
                 ylab = "Molar flow (mol/h)",
                 facet = 'event',
                 args = list(facet = list(scales = "free", ncol = 2, labeller = ggplot2::as_labeller(event_names())),
                             boxp = list(show.legend = F)
                             ),
                 custom_color = T
            ) +
            ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, fill = "white", size = 2.5) +
            ggplot2::stat_summary(fun = min, geom = "point", shape = 25, size = 2.5) +
            ggplot2::stat_summary(fun = max, geom = "point", shape = 19, size = 2.5) +
            ggplot2::theme(axis.text.y = ggplot2::element_text(color = 'black', size = 10),
                           axis.title.y = ggplot2::element_text(size = 12),
                           panel.spacing = ggplot2::unit(0.5, "cm"),
                           plot.margin = ggplot2::unit(c(0, 0, 2, 2), 'cm'),
                           panel.background = ggplot2::element_rect(colour = 'black'),
                           axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank()
                          )
      
      total_height <- 180 + 180 * length(input$graph_event)/2
      boxp <- plotly::ggplotly(p, height = total_height)

      boxp$x$data <- lapply(boxp$x$data, function(x) {
        if (x$type == "box") {
          x$q1 <- min(x$y)
          x$q3 <- max(x$y)
          x$boxpoints <- FALSE
          x$whiskerwidth <- 0
        }
        return(x)
      })

      boxp

    })

    return(list(
      chem_values = chem_values,
      molar_flow = mf,
      conversion = conversion,
      mass_balance = mb,
      boxplot = boxplot_data
    ))

  })
}

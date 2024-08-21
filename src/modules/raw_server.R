raw_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    names <- shiny::reactive({
      app_state$bd() %>%
      dplyr::summarise(name = unique(name), .by = event) %>%
      dplyr::mutate(name = stringr::str_c("Event: ", event, " - ", name)) %>%
      {setNames(.$name, .$event)}
    })

    output$UIcompunds <- shiny::renderUI({
      shiny::req(app_state$gc)

      comp <- colnames(app_state$gc())[-c(1:5)] %>% change_str("_", " ")

      shinyWidgets::pickerInput(
        inputId = ns("compounds"), label = "Select compounds to plot",
        choices = comp, multiple = TRUE, selected = comp,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
      )
    })

    output$xgc <- shiny::renderUI({
      shiny::selectInput(inputId = ns('gc_xaxis'), label = 'Select x axis',
                  choices = c("Time" = "time",
                              "Temperature" = "tic_300_pv"))
    })

    output$gc_events <- shiny::renderUI({
      shiny::req(app_state$gc)

      selected <- app_state$gc() %>%
        dplyr::filter(n() > 1, .by = event) %>%
        dplyr::pull(event) %>% unique()

      shinyWidgets::pickerInput(
        inputId = ns("gc_event"), label = "Select events to plot",
        choices = unique(app_state$gc()$event), multiple = TRUE, selected = selected,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    output$composition <- plotly::renderPlotly({
      shiny::req(input$gc_event)
      
      comp_plot <- app_state$gc() %>%
        dplyr::select(-injection) %>%
        tidyr::pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(Compound = change_str(Compound, "_", " ")) %>%
        dplyr::filter(Compound %in% input$compounds & event %in% input$gc_event) %>%
        plot(x = .data[[input$gc_xaxis]],
             y = value,
             fill = Compound,
             facet = "event",
             ylab = "Composition",
             area = T, 
             hline = T, 
             xlab = ifelse(input$gc_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'), 
             args = list(area = list(alpha = 0.6, color = 'black', linewidth = 0.2),
                         hline = list(yintercept = 100, linetype = 'dashed'),
                         facet = list(ncol = 2, labeller = ggplot2::as_labeller(names()), scales = 'free_x'))) +
          ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
                         axis.title = ggplot2::element_text(size = 12),
                         panel.spacing = ggplot2::unit(1, "lines"),
                         plot.margin = ggplot2::unit(c(0, 0, 2, 0), 'cm'),
                         panel.background = ggplot2::element_rect(colour = 'black'))
      
        total_height <- 180 + 180 * length(input$gc_event)
      
      plotly::ggplotly(comp_plot, height = total_height, dynamicTicks = T, tooltip = "fill")
    })

    ## ms file -------------------------------------------------------------------------------------

    output$xms <- shiny::renderUI({
      shiny::conditionalPanel("input.tabset3 == 'Events'",
                        shiny::selectInput(inputId = ns('ms_xaxis'), label = 'Select x axis:',
                                           choices = c("Time" = "time", "Temperature" = "tic_300_pv"))
                        )
    })

    output$yms <- shiny::renderUI({
      shiny::req(app_state$ms)

      choices <- app_state$ms() %>%
        dplyr::select(contains('_amu_')) %>%
        colnames() %>%
        stringr::str_replace_all('_', ' ')

      shinyWidgets::pickerInput(
        inputId = ns("ms_yaxis"), label = "Select compound:", multiple = T,
        choices = choices, selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })

    output$ms_events <- shiny::renderUI({
      shiny::req(app_state$ms)
      
      selected <- app_state$ms() %>%
        dplyr::filter(n() > 1, .by = event) %>%
        dplyr::pull(event) %>% unique()

      shiny::conditionalPanel("input.tabset3 == 'Events'",
                        shinyWidgets::pickerInput(
                          inputId = ns("ms_event"), label = "Select events to plot",
                          choices = unique(app_state$ms()$event), multiple = TRUE, selected = selected,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        )
        )
    })

    output$msplot_gen <- dygraphs::renderDygraph({
      shiny::req(input$ms_yaxis)

      data <- app_state$ms() %>%
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('_amu_')) %>%
        plotly::select(time_absolute_date_time, dplyr::contains(input$ms_yaxis)) %>%
        dplyr::mutate(dplyr::across(.cols = dplyr::contains('amu'),
                      .fns = ~smooth.spline(.x, spar = input$smooth)$y)) %>%
        dygraphs::dygraph(xlab = ifelse(input$ms_xaxis == 'tic_300_pv',
                              'Temperature (°C)', 'Reaction Time (min)'))


      dygraphs::dySeriesData(data, 'plot', input$ms_yaxis) %>%
        dygraphs::dyRangeSelector() %>%
        dygraphs::dyOptions(useDataTimezone = TRUE) %>%
        dygraphs::dyOptions(strokeWidth = 2) %>%
        dygraphs::dyLegend(width = 450)

    })

    output$msplot <- plotly::renderPlotly({
      shiny::req(input$ms_yaxis)
      dates <- lubridate::ymd_hms(input$msplot_gen_date_window)

      ms_plot <- app_state$ms() %>%
        dplyr::filter(dplyr::between(time_absolute_date_time, dates[1], dates[2]) &
                event %in% input$ms_event) %>%
        tidyr::pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(value = smooth.spline(value, spar = input$smooth)$y, .by = Compound) %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ')) %>%
        dplyr::filter(Compound %in% input$ms_yaxis) %>%
        tidyr::drop_na(event) %>%
        plot(x = .data[[input$ms_xaxis]],
             y = value,
             color = Compound,
             lines = T,
             xlab = ifelse(input$ms_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
             ylab = "Arbitrary unit (a.u)",
             facet = 'event',
             args = list(facet = list(scales = ifelse(input$scale == TRUE, 'fixed', 'free_x'),
                                      ncol = 2, labeller = ggplot2::as_labeller(names())))) +
          ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
                         axis.title = ggplot2::element_text(size = 12),
                         panel.spacing = ggplot2::unit(1, "lines"),
                         panel.background = ggplot2::element_rect(colour = 'black'),
                         plot.margin = ggplot2::unit(c(0, 0, 2, 2), units = 'cm'))

      total_height <- 180 + 180 * length(input$ms_event)
      plotly::ggplotly(ms_plot, height = total_height, dynamicTicks = T, tooltip = c('color', 'x', 'y'))
    })

    fit <- shiny::reactive({
      shiny::req(input$msplot_gen_date_window)
      shiny::req(input$ms_yaxis)

      range <- input$msplot_gen_date_window

      app_state$ms() %>%
        dplyr::filter(dplyr::between(time_absolute_date_time, lubridate::ymd_hms(range[1]), lubridate::ymd_hms(range[2]))) %>%
        dplyr::rename_with(.fn = ~stringr::str_replace_all(.x, '_', ' '), .cols = dplyr::contains('_amu_')) %>%
        dplyr::pull(.data[[input$ms_yaxis[1]]]) %>%
        smooth.spline(spar = input$smooth)
    })

    output$ms_int <- shiny::renderUI({
      shiny::req(input$msplot_gen_date_window)

      smooth <- \(x) predict(fit(), x)$y
      value <- integrate(smooth, 1, length(fit()$x))$value

      shiny::div(
        shiny::span('Integral value:', style = 'font-weight: bold'),
        shiny::span(prettyunits::pretty_num(value), 'u.m.a')
      )
    })

    output$int_plot <- shiny::renderPlot({
      shiny::req(input$msplot_gen_date_window)

      par(mar = c(5, 4, 1.5, 2))
      base::plot(x = fit()$x, y = fit()$y, type = 'l', bty = 'n', xlab = '', ylab = '')
    })
    }
)}

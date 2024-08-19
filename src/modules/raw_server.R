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

      comp <- colnames(app_state$gc())[-c(1:5)] %>%
        stringr::str_replace_all('_', ' ') %>%
        stringr::str_to_title()

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
      
      plot <- app_state$gc() %>%
        dplyr::select(-injection) %>%
        tidyr::pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ') %>% stringr::str_to_title()) %>%
        dplyr::filter(Compound %in% input$compounds & event %in% input$gc_event) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data[[input$gc_xaxis]], y = value, fill = Compound)) +
        ggplot2::geom_area(alpha = 0.6, color = 'black', linewidth = 0.2) +
        ggplot2::geom_hline(yintercept = 100, linetype = 'dashed') +
        ggplot2::labs(x = ifelse(input$gc_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
                      y = "Composition") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
              axis.title = ggplot2::element_text(size = 12),
              panel.spacing = ggplot2::unit(1, "lines"),
              plot.margin = ggplot2::unit(c(0, 0, 2, 0), 'cm'),
              panel.background = ggplot2::element_rect(colour = 'black'))

      total_height <- 500
      
      if(input$gc_type == 'Events') {
        plot <- plot + 
          ggplot2::facet_wrap(~event, scales = 'free', ncol = 2,
                             labeller = ggplot2::as_labeller(names())) 
        total_height <- 180 + 180 * length(input$gc_event)
      }
      
      plotly::ggplotly(plot, height = total_height, dynamicTicks = T, tooltip = "fill")
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

      plot <- app_state$ms() %>%
        dplyr::filter(dplyr::between(time_absolute_date_time, dates[1], dates[2]) &
                event %in% input$ms_event) %>%
        tidyr::pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
        dplyr::mutate(value = smooth.spline(value, spar = input$smooth)$y, .by = Compound) %>%
        dplyr::mutate(Compound = stringr::str_replace_all(Compound, '_', ' ')) %>%
        dplyr::filter(Compound %in% input$ms_yaxis) %>%
        tidyr::drop_na(event) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data[[input$ms_xaxis]], y = value, color = Compound)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = ifelse(input$ms_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
                      y = "Arbitrary unit (a.u)") +
        ggplot2::facet_wrap(~event, scales = ifelse(input$scale == TRUE, 'fixed', 'free'),
                            ncol = 2, labeller = ggplot2::as_labeller(names())) +
        ggplot2::theme_bw()  +
        ggplot2::theme(axis.text = ggplot2::element_text(color = 'black', size = 10),
                       axis.title = ggplot2::element_text(size = 12),
                       panel.spacing = ggplot2::unit(1, "lines"),
                       panel.background = ggplot2::element_rect(colour = 'black'),
                       plot.margin = ggplot2::unit(c(0, 0, 2, 2), units = 'cm'))

      total_height <- 180 + 180 * length(input$ms_event)
      plotly::ggplotly(plot, height = total_height,
               dynamicTicks = T, tooltip = c('color', 'x', 'y'))
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
      plot(x = fit()$x, y = fit()$y, type = 'l',
            bty = 'n', xlab = '', ylab = '')
    })
    }
)}

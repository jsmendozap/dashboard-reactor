raw_server <- function(id, bd, gc, ms) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)
    if(!is.null(gc)){

      output$UIcompunds <- renderUI({
        
        comp <- colnames(gc)[-c(1:5)] %>%
          str_replace_all('_', ' ') %>% 
          str_to_title()
        
        pickerInput(
          inputId = ns("compounds"), label = "Select compounds to plot", 
          choices = comp, multiple = TRUE, selected = comp,
          options = list(`actions-box` = TRUE, `live-search` = TRUE),
        )
      })
      
      output$xgc <- renderUI({
        selectInput(inputId = ns('gc_xaxis'), label = 'Select x axis',
                    choices = c("Time" = "time",
                                "Temperature" = "tic_300_pv"))
      })
      
      output$gc_events <- renderUI({
        
        selected <- gc %>% 
          filter(n() > 1, .by = event) %>%
          pull(event) %>% unique()
        
        pickerInput(
          inputId = ns("gc_event"), label = "Select events to plot", 
          choices = unique(gc$event), multiple = TRUE, selected = selected,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        )
      })
      
      output$composition <- renderPlotly({
        req(input$gc_event)
        
        plot <- gc %>%
          select(-injection) %>%
          pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
          mutate(Compound = str_replace_all(Compound, '_', ' ') %>% str_to_title()) %>%
          filter(Compound %in% input$compounds & event %in% input$gc_event) %>%
          ggplot(aes(x = .data[[input$gc_xaxis]], y = value, fill = Compound)) +
          geom_area(alpha = 0.6, color = 'black', linewidth = 0.2) +
          geom_hline(yintercept = 100, linetype = 'dashed') +
          labs(x = ifelse(input$gc_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
               y = "Composition") +
          facet_wrap(~event, scales = 'free', ncol = 2) +
          theme_bw() +
          theme(axis.text = element_text(color = 'black', size = 10),
                axis.title = element_text(size = 12),
                panel.background = element_rect(colour = 'black'))
      
        ggplotly(plot, dynamicTicks = T, tooltip = "fill")
          
      })
    }
  
    ## ms file -------------------------------------------------------------------------------------
    
    if(!is.null(ms)) {

      output$xms <- renderUI({
        conditionalPanel("input.tabset3 == 'Events'",
                         selectInput(inputId = ns('ms_xaxis'), label = 'Select x axis:',
                                     choices = c("Time" = "time",
                                     "Temperature" = "tic_300_pv"))  
                         )
      })
      
      output$yms <- renderUI({
        
        choices <- ms %>%
          select(contains('_amu_')) %>%
          colnames() %>%
          str_replace_all('_', ' ')
        
        pickerInput(
          inputId = ns("ms_yaxis"), label = "Select compound:", multiple = T,
          choices = choices, selected = choices[1],
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        )
      })
      
      output$ms_events <- renderUI({
        
        selected <- ms %>% 
          filter(n() > 1, .by = event) %>%
          pull(event) %>% unique()
        
        conditionalPanel("input.tabset3 == 'Events'",
                         pickerInput(
                           inputId = ns("ms_event"), label = "Select events to plot", 
                           choices = unique(ms$event), multiple = TRUE, selected = selected,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE)
                         )  
          )
      })
      
      output$msplot_gen <- renderDygraph({
        req(input$ms_yaxis)
        
        data <- ms %>%
          rename_with(.fn = ~str_replace_all(.x, '_', ' '), .cols = contains('_amu_')) %>%
          select(time_absolute_date_time, contains(input$ms_yaxis)) %>%
          mutate(across(.cols = contains('amu'),
                        .fns = ~smooth.spline(.x, spar = input$smooth)$y)) %>%
          dygraph(xlab = ifelse(input$ms_xaxis == 'tic_300_pv',
                                'Temperature (°C)', 'Reaction Time (min)')) 
        
        dySeriesData(data, 'plot', input$ms_yaxis) %>%
          dyRangeSelector() %>%
          dyOptions(useDataTimezone = TRUE) %>%
          dyOptions(strokeWidth = 2) %>%
          dyLegend(width = 450)
          
      })
      
      output$msplot <- renderPlotly({
        req(input$ms_yaxis)
        dates <- ymd_hms(input$msplot_gen_date_window) 
        
        plot <- ms %>%
          filter(between(time_absolute_date_time, dates[1], dates[2]) &
                 event %in% input$ms_event) %>%
          pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
          mutate(value = smooth.spline(value, spar = input$smooth)$y, .by = Compound) %>%
          mutate(Compound = str_replace_all(Compound, '_', ' ')) %>% 
          filter(Compound %in% input$ms_yaxis) %>%
          drop_na(event) %>%
          ggplot(aes(x = .data[[input$ms_xaxis]], y = value, color = Compound)) +
          geom_line() +
          labs(x = ifelse(input$ms_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
               y = "Signal detected (a.m.u)") +
          facet_wrap(~event, scales = 'free', ncol = 2) +
          theme_bw()  +
          theme(axis.text = element_text(color = 'black', size = 10),
                axis.title = element_text(size = 12),
                panel.background = element_rect(colour = 'black'),
                plot.margin = unit(c(0, 0, 0.2, 0.2), units = 'inches'))
        
        ggplotly(plot, dynamicTicks = T, tooltip = c('color', 'x', 'y'))
      })
      
      fit <- reactive({
        req(input$msplot_gen_date_window)
        req(input$ms_yaxis)
        
        range <- input$msplot_gen_date_window
        
        ms %>%
          filter(between(time_absolute_date_time, ymd_hms(range[1]), ymd_hms(range[2]))) %>%
          rename_with(.fn = ~str_replace_all(.x, '_', ' '), .cols = contains('_amu_')) %>%
          pull(.data[[input$ms_yaxis[1]]]) %>%
          smooth.spline(spar = input$smooth)
      })
      
      output$ms_int <- renderUI({
        req(input$msplot_gen_date_window)
        
        smooth <- \(x) predict(fit(), x)$y
        value <- integrate(smooth, 1, length(fit()$x))$value
        
        div(
          span('Integral value:', style = 'font-weight: bold'),
          span(pretty_num(value), 'u.m.a')
        )
      })
      
      output$int_plot <- renderPlot({
        req(input$msplot_gen_date_window)
        
        par(mar = c(5, 4, 1.5, 2))
        plot(x = fit()$x, y = fit()$y, type = 'l',
             bty = 'n', xlab = '', ylab = '')
      })
    }
  }
)}
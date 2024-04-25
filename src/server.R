options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output) {
  df <- reactive({
    req(input$reactor)
    load_file(input$reactor$datapath)
  })
  
  bd <- reactive({
    df() %>% filter(n >= input$log_events)
  })
  
  ## Log -----------------------------------------------------------------------
  
  selected1 <- reactive({ input$var })
  selected2 <- reactive({ input$var2 })
  
  output$dygraph <- renderDygraph({ 
    renderdy(bd(), selected1(), input, 'legend') 
  })
  
  output$dygraph2 <- renderDygraph({
    renderdy(bd(), selected2(), input, 'legend2') 
    })
  
  output$log <- renderDT({
    bd() %>%
      slice(1, .by = event) %>%
      select(event, date_time, name, time_duration) %>%
      rename('Event' = 1, 'Start time' = 2, 'Event name' = 3, 'Duration' = 4) %>%
      datatable(options = list(pageLength = 5), selection = 'single')
    })
  
  output$valve <- renderText({
    req(input$log_rows_selected)
    
    pos <- bd() %>%
      slice_head(n = 1, by = event) %>%
      pull(rswitch_val)
    
    paste('Valve position:', pos[input$log_rows_selected])
  })
  
  output$fi_110 <- renderValueBox({
    reactor_values(bd(), 'fi_110', 'Air', input)
  })
  
  output$fi_120 <- renderValueBox({
    reactor_values(bd(), 'fi_120', 'Carbon dioxide', input)
  })
  
  output$fi_130 <- renderValueBox({
    reactor_values(bd(), 'fi_130', 'Argon / Propane', input)
  })
  
  output$fi_140 <- renderValueBox({
    reactor_values(bd(), 'fi_140', 'Nitrogen', input)
  })
  
  ## Quality control -----------------------------------------------------------
  
  output$flow <- renderDT(bd() %>% 
                            summarise(across(.cols = c(2, 4, 6, 8), .fn = \(x) round(sum(x)/n(), 1)),
                                      .by = event) %>%
                            select(-1) %>%
                            rowwise() %>%
                            mutate(total = sum(fi_110, fi_120, fi_130, fi_140)) %>%
                            rename('Air (mL/min)' = 1, 'Carbon dioxide (mL/min)' = 2,
                                   'Argon / Propane (mL/min)' = 3, 'Nitrogen (mL/min)' = 4,
                                   'Total flow (mL/min)' = 5),
                          selection = 'single')
  
  
  output$corr <- renderDT({
    bd() %>% 
      summarise(cor_air = mean(((fi_110 + 1)/(1 + fic_110))) %>% round(1),
                cor_co2 = mean(((fi_120 + 1)/(fic_120 + 1))) %>% round(1),
                cor_ar = mean(((fi_130 + 1)/(fic_130 +1))) %>% round(1),
                cor_n2 = mean(((fi_140 + 1)/(fic_140 + 1))) %>% round(1),
                .by = event) %>%
      select(-1) %>%
      rename('Air' = 1, 'Carbon dioxide' = 2, 'Argon/ propane' = 3, 'Nitrogen' = 4) %>%
      datatable(options = list(autoWidth = TRUE), selection = 'single') %>%
      formatStyle(c('Air', 'Carbon dioxide', 'Argon/ propane', 'Nitrogen'),
                  backgroundColor = styleInterval(c(0.9, 1.1), c('red', 'lightgreen', 'red')))
  })
  
  output$norm <- renderPlotly({
    
    plot <- tryCatch(expr = {
      
      bd() %>% 
        transmute(date_time, deriv = norm_deriv(normoliter_out)) %>% 
        group_by(group = rep(row_number(), each = 5, length.out = n())) %>%
        summarise(time = mean(date_time), 
                  value = mean(deriv, na.rm = T) %>% round(3)) %>% 
        ggplot() +
        geom_line(aes(x = time, y = value), linewidth = 0.2) +
        labs(x = "Time", y = "Normalized milimeters rate") +
        theme_bw()
      
    }, error = \(e) {
      ggplot(bd()) +
        geom_line(aes(x = date_time, y = normoliter_out)) +
        labs(x = "Time", y = "Normalized milimeters",
             title = 'Flow rate not possible') +
        theme_bw()
    })
      
    ggplotly(plot)
  })
  
  output$temp <- renderDT({
    bd() %>%
      summarise(name = name[1],
                rate = diff(tic_300_pv) %>% mean(na.rm = T) ,
                mean_measure = mean(tic_300_pv),
                mean_set = mean(tic_300_sp),
                mean_r1 = mean(te_310),
                mean_r2 = mean(te_320),
                .by = event) %>%
      select(-1) %>%
      mutate(across(.cols = 2:6, .fns = ~round(.x, 1))) %>%
      rename("Event" = 1, "Avg. measured" = 3, "Avg. setted" = 4,
             "Rate of change" = 2, "Avg. Reactor 1" = 5, "Avg. Reactor 2" = 6) %>%
      datatable(selection = 'single', options = list(pageLength = 5))
  })
  
  output$diffTemp <- renderPlotly({
    plot <- bd() %>%
              mutate(difference = tic_300_pv - te_320) %>%
              ggplot() +
              geom_line(aes(x = date_time, y = difference), linewidth = 0.2) +
              facet_wrap(~event, scales = 'free') +
              labs(x = "Time", y = "Temperature differences") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
    
    ggplotly(plot)
  })
  
  output$press <- renderDT({
    bd() %>% 
      summarise(name = name[1],
                mean_set = mean(p_set),
                mean_r1 = mean(pt_310),
                mean_r2 = mean(pt_320),
                delta = mean_r1 - mean_r2,
                .by = event) %>%
      select(-1) %>% 
      mutate(across(.cols = 2:5, .fns = ~round(.x, 1))) %>%
      rename("Event" = 1, "Avg. setted" = 2, "Avg. Reactor 1" = 3,
             "Avg. Reactor 2" = 4, "Delta of pressure" = 5) %>%
      datatable(selection = 'single')
  })
  
  output$plotPress <- renderDygraph({
    
    req(input$press_rows_selected)
    
    selected <- slice_head(bd(), n = 1, by = event) %>%
      filter(row_number() == input$press_rows_selected) %>%
      pull(event)
    
    bd() %>%
      filter(event == selected) %>%
      rowwise() %>%
      transmute(time = date_time, delta = mean(pt_310) - mean(pt_320)) %>%
      dygraph() %>%
      dySeries("delta") %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyLegend(labelsDiv = 'legend')
  })
  

## Raw data --------------------------------------------------------------------

  gc <- reactive({
    req(input$gc)
    load_gc(path = input$gc$datapath, bd = bd())
  })
  
  output$UIcompunds <- renderUI({
    comp <- colnames(gc())[-c(1:5)] %>%
      str_replace_all('_', ' ') %>% 
      str_to_title()
    
    pickerInput(
      inputId = "compounds", label = "Select compounds to plot", 
      choices = comp, multiple = TRUE, selected = comp,
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
    )
  })
  
  output$xgc <- renderUI({
    selectInput(inputId = 'gc_xaxis', label = 'Select x axis',
                choices = c("Time" = "time",
                            "Temperature" = "tic_300_pv"))
  })
  
  output$gc_events <- renderUI({
    
    selected <- gc() %>% 
      filter(n() > 1, .by = event) %>%
      pull(event) %>% unique()
    
    pickerInput(
      inputId = "gc_event", label = "Select events to plot", 
      choices = unique(gc()$event), multiple = TRUE, selected = selected,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  output$composition <- renderPlotly({
    req(input$gc_event)
    
    plot <- gc() %>%
      select(-injection) %>%
      pivot_longer(cols = 5:ncol(.), names_to = 'Compound', values_to = 'value') %>%
      mutate(Compound = str_replace_all(Compound, '_', ' ') %>% str_to_title()) %>%
      filter(Compound %in% input$compounds & event %in% input$gc_event) %>%
      ggplot(aes(x = .data[[input$gc_xaxis]], y = value, fill = Compound)) +
      geom_area(alpha = 0.6, color = 'black', linewidth = 0.2) +
      labs(x = ifelse(input$gc_xaxis == 'tic_300_pv','Temperature (°C)', 'Reaction time (min)'),
           y = "Composition") +
      facet_wrap(~event, scales = 'free', ncol = 2) +
      theme_bw() 
  
    ggplotly(plot, dynamicTicks = T, tooltip = "fill")
      
  })

  ms <- reactive({
    req(input$ms)
    load_ms(path = input$ms$datapath, bd = bd())
  })
  
  output$xms <- renderUI({
    selectInput(inputId = 'ms_xaxis', label = 'Select x axis:',
                choices = c("Time" = "time",
                            "Temperature" = "tic_300_pv"))
  })
  
  output$yms <- renderUI({
    
    choices <- ms() %>%
      select(contains('_amu_')) %>%
      colnames() %>%
      str_replace_all('_', ' ')
    
    pickerInput(
      inputId = "ms_yaxis", label = "Select compound:", multiple = T,
      choices = choices, selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  output$startms <- renderUI({
    airDatepickerInput('mstime', label = 'Start time:',
                       value = ms()$time_absolute_date_time[1],
                       timepickerOpts = list("timeFormat" = "HH:mm"),
                       timepicker = T)
  })
  
  output$msplot_gen <- renderDygraph({
    req(input$ms_yaxis)
    
    ms() %>%
      filter(time_absolute_date_time >= ymd_hms(input$mstime)) %>%
      rename_with(.fn = ~str_replace_all(.x, '_', ' '), .cols = contains('_amu_')) %>%
      transmute(time_absolute_date_time, 
                amu = smooth.spline(.[,input$ms_yaxis[1]], spar = input$smooth)$y) %>%
      dygraph(xlab = ifelse(input$ms_xaxis == 'tic_300_pv',
                            'Temperature (°C)', 'Reaction Time (min)')) %>%
      dySeries('amu') %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyLegend(width = 450)
  })
  
  output$msplot <- renderPlotly({
    req(input$ms_yaxis)
    
    plot <- ms() %>%
      filter(time_absolute_date_time >= ymd_hms(input$mstime)) %>%
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
      theme_bw() 
    
    ggplotly(plot, dynamicTicks = T, tooltip = 'color')
  })
  
  fit <- reactive({
    req(input$msplot_gen_date_window)
    req(input$ms_yaxis)
    
    range <- input$msplot_gen_date_window
    
    ms() %>%
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
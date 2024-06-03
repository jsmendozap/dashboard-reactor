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
  
  output$log <- renderReactable({
    bd() %>%
      slice_head(n = 1, by = event) %>%
      select(event, date_time, name, time_duration) %>%
      custom_reactable(
        columns = list(
          event = colDef(name = 'Event', width = 100),
          date_time = colDef(name = 'Start time', width = 200),
          name = colDef(name = 'Event name', minWidth = 200),
          time_duration = colDef(name = 'Duration', width = 100)
        ), selection = 'single'
      )
  })
  
  output$valve <- renderText({
    req(getReactableState('log', 'selected'))
    
    sel <- getReactableState('log', 'selected')
    pos <- bd() %>%
      slice_head(n = 1, by = event) %>%
      pull(rswitch_val)
    
    paste('Valve position:', pos[sel])
  })
  
  output$fi_110 <- renderValueBox({
    reactor_values(bd(), 'fi_110', 'Air')
  })
  
  output$fi_120 <- renderValueBox({
    reactor_values(bd(), 'fi_120', 'Carbon dioxide')
  })
  
  output$fi_130 <- renderValueBox({
    reactor_values(bd(), 'fi_130', 'Argon / Propane')
  })
  
  output$fi_140 <- renderValueBox({
    reactor_values(bd(), 'fi_140', 'Nitrogen')
  })
  
  ## Quality control -----------------------------------------------------------
  
  output$flow <- renderReactable({
    bd() %>% 
      summarise(across(.cols = c(2, 4, 6, 8),
                       .fn = \(x) round(sum(x)/n(), 1)),
                .by = event) %>%
      select(-1) %>%
      rowwise() %>%
      mutate(total = sum(fi_110, fi_120, fi_130, fi_140)) %>%
      custom_reactable(
        columns = list(
          fi_110 = colDef(name = 'Air (mL/min)'),
          fi_120 = colDef(name = 'Carbon dioxide (mL/min)'),
          fi_130 = colDef(name = 'Argon / Propane (mL/min)'),
          fi_140 = colDef(name = 'Nitrogen (mL/min)'),
          total = colDef(name = 'Total flow (mL/min)')
        ), 
      )
  }) 
  
  output$corr <- renderReactable({
    bd() %>% 
      summarise(cor_air = mean(((fi_110 + 1)/(1 + fic_110))) %>% round(1),
                cor_co2 = mean(((fi_120 + 1)/(fic_120 + 1))) %>% round(1),
                cor_ar = mean(((fi_130 + 1)/(fic_130 +1))) %>% round(1),
                cor_n2 = mean(((fi_140 + 1)/(fic_140 + 1))) %>% round(1),
                .by = event) %>%
      select(-1) %>%
      custom_reactable(
        columns = list(
          cor_air = colDef(name = 'Air', style = conditional_color),
          cor_co2 = colDef(name = 'Carbon dioxide', style = conditional_color),
          cor_ar = colDef(name = 'Argon / Propane', style = conditional_color),
          cor_n2 = colDef(name = 'Nitrogen', style = conditional_color)
        )
      )
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
  
  output$temp <- renderReactable({
    suppressWarnings({
      
    bd() %>%
      summarise(name = name[1],
                rate = diff(tic_300_pv) %>% mean(na.rm = T) ,
                mean_measure = mean(tic_300_pv),
                mean_set = mean(tic_300_sp),
                mean_r1 = mean(te_310),
                mean_r2 = mean(te_320),
                diff = list(tic_300_pv - te_320),
                .by = event)  %>%
      select(-1) %>%
      mutate(across(.cols = 2:6, .fns = ~round(.x, 1))) %>%
      custom_reactable(
        columns = list(
          name = colDef(name = 'Event', minWidth = 200),
          rate = colDef(name = 'Rate of change'),
          mean_measure = colDef(name = 'Avg. measured'),
          mean_set = colDef(name = 'Avg. setted'),
          mean_r1 = colDef(name = 'Avg. Reactor 1'),
          mean_r2 = colDef(name = 'Avg. Reactor 2'),
          diff = colDef(name = 'Temperature difference', minWidth = 350,
                        cell = react_sparkline(., show_area = T, decimals = 2,
                                               line_color = 'darkblue'))),
        rowStyle = JS("function(rowInfo) { return { height: '50px' }}")
      )
    })
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
  
  output$press <- renderReactable({
    bd() %>% 
      summarise(name = name[1],
                mean_set = mean(p_set),
                mean_r1 = mean(pt_310),
                mean_r2 = mean(pt_320),
                delta = mean_r1 - mean_r2,
                .by = event) %>%
      select(-1) %>% 
      mutate(across(.cols = 2:5, .fns = ~round(.x, 1))) %>%
      custom_reactable(
        columns = list(
          name = colDef(name = 'Event', minWidth = 200),
          mean_set = colDef(name = 'Avg. setted', minWidth = 80),
          mean_r1 = colDef(name = 'Avg. Reactor 1', minWidth = 80),
          mean_r2 = colDef(name = 'Avg. Reactor 2', minWidth = 80),
          delta = colDef(name = 'Delta of pressure', minWidth = 80)
        ), selection = 'single'
      )
  })
  
  output$plotPress <- renderDygraph({
    
    req(getReactableState('press', 'selected'))
    
    selected <- slice_head(bd(), n = 1, by = event) %>%
      filter(row_number() == getReactableState('press', 'selected')) %>%
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
  
## Chemometric -----------------------------------------------------------------

  output$std <- renderReactable({
    
    tecq <- c('Ramp', 'TOS', 'regeneration', 'CO-TPD', 'Propane-TPD', 'H2-TPR')
    is <- c('Argon', 'Nitrogen')
    
    bd() %>%
      slice_head(n = 1, by = event) %>%
      select(event, name) %>%
      mutate(technique = list(tecq), is = list(is), qis = NA) %>%
      custom_reactable(selection = 'single', 
                       columns = list(
                         event = colDef(name = 'Event'),
                         name = colDef(name = 'Event name', minWidth = 300),
                         technique = colDef(name = 'Technique', minWidth = 150,
                                            cell = dropdown_extra(id = 'dropdown',
                                                              choices = tecq,
                                                              class = 'dropdown-extra')),
                         is = colDef(name = 'Internal Standard', minWidth = 150,
                                     cell = dropdown_extra(id = 'dropdown2', choices = is,
                                                           class = 'dropdown-extra')),
                         qis = colDef(name = 'QIS', minWidth = 200, cell = text_extra('qis'))
                       ), style = "border-radius: '3px'"
                      )
  })
  
  observeEvent(input$dropdown, print(input$dropdown))
}

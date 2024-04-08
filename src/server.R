source("src/auxiliar.R")

server <- function(input, output) {
  bd <- reactive({
    req(input$file)
    load_file(input$file$datapath)
  })
  
  selected <- reactive({ input$var })
  
  output$dygraph <- renderDygraph({
    plot <- dygraph(bd() %>% select(1, selected())) %>%
      dySeries(selected()) %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyCSS("dygraph.css") %>%
      dyLegend(labelsDiv = 'legend')
    
    if(!is.null(input$log_rows_selected)){
      time <- bd() %>% 
        filter(event == input$log_rows_selected) %>%
        slice(1, n()) %>%
        pull(date_time)
      
      plot %>% dyShading(from = time[1], to = time[2], color = "#C1BEE1") 
      
    } else {
      plot
    }
  })
  
  output$log <- renderDT(bd() %>%
                          slice(1, .by = event) %>%
                          select(date_time, name, n) %>%
                          rename('Date' = 1, 'Event' = 2, 'Duration' = 3),
                         selection = 'single')
  
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
    
    plot <- bd() %>% 
      transmute(date_time, deriv = norm_deriv(normoliter_out)) %>% 
      group_by(group = rep(row_number(), each = 5, length.out = n())) %>%
      summarise(time = mean(date_time), 
                value = mean(deriv, na.rm = T) %>% round(3)) %>% 
      ggplot() +
      geom_line(aes(x = time, y = value), linewidth = 0.2) +
      labs(x = "Time", y = "Normalized milimeters rate") +
      theme_bw()
      
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
      datatable(selection = 'single')
  })
  
  
  output$plotTemp <- renderPlotly({
    plot <- ggplot(bd()) +
      geom_point(aes(x = tic_300_sp, y = tic_300_pv), size = 0.5) +
      labs(x = "Setted temperature", y = "Measured temperature") +
      theme_bw()
    
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
    
    bd() %>%
      filter(event == input$press_rows_selected) %>%
      rowwise() %>%
      transmute(time = date_time, delta = mean(pt_310) - mean(pt_320)) %>%
      dygraph() %>%
      dySeries("delta") %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyLegend(labelsDiv = 'legend')
  })
  
}
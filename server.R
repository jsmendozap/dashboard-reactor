source("auxiliar.R")

server <- function(input, output) {
  selected <- reactive({ input$var })
  
  output$dygraph <- renderDygraph({
    
    plot <- dygraph(bd %>% select(1, selected())) %>%
      dySeries(selected()) %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyCSS("dygraph.css") %>%
      dyLegend(labelsDiv = 'legend')
    
    if(!is.null(input$log_rows_selected)){
      time <- bd %>% 
        filter(event == input$log_rows_selected) %>%
        slice(1, n()) %>%
        pull(date_time)
      
      plot %>% dyShading(from = time[1], to = time[2], color = "#C1BEE1") 
      
    } else {
      plot
    }
  })
  
  output$log <- renderDT(bd %>%
                          slice(1, .by = event) %>%
                          select(1, 20, 16) %>%
                          rename('Date' = 1, 'Event' = 2, 'Duration' = 3))
  
  output$flow <- renderDT(bd %>% 
                            summarise(across(.cols = c(2, 4, 6, 8), .fn = \(x) round(sum(x), 1)),
                                      .by = event) %>%
                            select(-1) %>%
                            rowwise() %>%
                            mutate(total = sum(fi_110, fi_120, fi_130, fi_140)) %>%
                            rename('Air (mL)' = 1, 'Carbon dioxide (mL)' = 2,
                                   'Argon / Propane (mL)' = 3, 'Nitrogen (mL)' = 4,
                                   'Total flow (mL)' = 5))
  
  
  output$corr <- renderDT({
    bd %>% 
      summarise(cor_air = mean(((fi_110 + 1)/(1 + fic_110))) %>% round(1),
                cor_co2 = mean(((fi_120 + 1)/(fic_120 + 1))) %>% round(1),
                cor_ar = mean(((fi_130 + 1)/(fic_130 +1))) %>% round(1),
                cor_n2 = mean(((fi_140 + 1)/(fic_140 + 1))) %>% round(1),
                .by = event) %>%
      select(-1) %>%
      rename('Air' = 1, 'Carbon dioxide' = 2, 'Argon/ propane' = 3, 'Nitrogen' = 4) %>%
      datatable(options = list(autoWidth = TRUE)) %>%
      formatStyle(c('Air', 'Carbon dioxide', 'Argon/ propane', 'Nitrogen'),
                  backgroundColor = styleInterval(c(0.9, 1.1), c('red', 'lightgreen', 'red')))
  })
  
  output$norm <- renderPlotly({
    
    plot <- bd %>% 
      summarise(time = date_time[1:(nrow(.) - input$lag)], y = norm_deriv(normoliter_out, input$lag)) %>%
      ggplot() +
        geom_line(aes(x = time, y = y), linewidth = 0.2) +
        labs(x = "Time", y = "Normalized milimeters") +
        theme_bw()
      
    ggplotly(plot)
  })
}
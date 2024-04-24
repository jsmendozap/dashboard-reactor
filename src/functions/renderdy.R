### Render dygraph -------------------------------------------------------------

renderdy <- function(bd, selected, input, legend) {
  suppressWarnings({
    plot <- dygraph(bd %>% select(1, all_of(selected)), group = 'log') %>%
      dySeries(selected) %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyCSS("dygraph.css") %>%
      dyLegend(labelsDiv = legend)
    
    if(!is.null(input$log_rows_selected)){
      
      selected <- slice_head(bd, n = 1, by = event) %>%
        filter(row_number() == input$log_rows_selected) %>%
        pull(event)
      
      time <- bd %>% 
        filter(event == selected) %>%
        slice(1, n()) %>%
        pull(date_time)
      
      plot %>% dyShading(from = time[1], to = time[2], color = "#C1BEE1") 
      
    } else {
      plot
    }
  })
}
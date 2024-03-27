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
                          select(1, 16, 12) %>%
                          rename('Date' = 1, 'Event' = 2, 'Duration' = 3))
}
reactor_values <- function(data, fi, comp, input){
  req(input$log_rows_selected)
  
  value <- data %>%
    slice_head(n = 1, by = event) %>%
    pull(fi)
  
  valueBox(span('\n', comp,
                style = 'font-size: 18px; font-weight: 600'),
           span(value[input$log_rows_selected], " mL/min", 
                style = 'font-size: 16px; font-weight: 500'))
}
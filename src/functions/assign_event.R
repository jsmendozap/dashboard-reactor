### Split and label events in file ---------------------------------------------

assign_event <- function(air, co2, ar, n2, val){
  event_count <- rep(NA, length(n2))
  event <- 0
  
  for (i in 1:length(n2)) {
    
    if (i == 1) {
      
      event <- event + 1
      event_count[i] = event  
      
    } else if (any(n2[i] != n2[i - 1],
                  air[i] != air[i - 1],
                  co2[i] != co2[i - 1],
                  ar[i] != ar[i - 1])) {
      
      event <- event + 1
      event_count[i] = event
      
    } else if(val[i] != val[i - 1] && n2[i] != 100) {
      
        event <- event + 1
        event_count[i] = event
    }
  }
  
  data.frame(event = event_count) %>% fill(event) %>% pull(event)
}
  
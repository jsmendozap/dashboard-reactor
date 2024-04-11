### Split and label events in file ---------------------------------------------

assign_event <- function(air, co2, ar, n2, val){
  event_count <- c(rep(NA, length(n2)))
  
  event <- 0
  first_count <- T
  second_count <- T
  third_count <- T
  
  for (i in 1:length(n2)) {
    
    if (i == 1 | n2[i] == 0 & air[i] == 0 & co2[i] == 0 & ar[i] == 0) {
      
      if (n2[i] == 0 & air[i] == 0 & co2[i] == 0 & ar[i] == 0) {
        if (first_count) {
          event <- event + 1
          first_count <- F
          second_count <- T
          third_count <- T
        }
        event_count[i] = event  
        next
      } else if (n2[i] == 0 & air[i] > 0 | co2[i] > 0 | ar[i] > 0) {
        if (first_count) {
          event <- event + 1
          first_count <- F
          second_count <- T
          third_count <- T
        }
        event_count[i] = event  
        next
      }
    } 
    
    if (n2[i] > 0 & air[i] == 0 & co2[i] == 0 & ar[i] == 0) {
      if (second_count) {
        event <- event + 1
        first_count <- T
        second_count <- F
        third_count <- T
      }
      
      event_count[i] = event
      
    } else if (n2[i] - n2[i - 1] != 0 | val[i] != val[i - 1]) {
      
      if(val[i] != val[i - 1]) {third_count <- T}
      
      if (third_count) {
        event <- event + 1
        first_count <- T
        second_count <- T
        third_count <- F
      }
      
      event_count[i] = event
      
    }
  }
  
  data.frame(event = event_count) %>% fill(event) %>% pull(event)
}

### Render dygraph -------------------------------------------------------------

renderdy <- function(bd, selected, input, legend) {
  plot <- dygraph(bd %>% select(1, selected), group = 'log') %>%
    dySeries(selected) %>%
    dyRangeSelector() %>%
    dyOptions(useDataTimezone = TRUE) %>%
    dyCSS("dygraph.css") %>%
    dyLegend(labelsDiv = legend)
  
  if(!is.null(input$log_rows_selected)){
    time <- bd %>% 
      filter(event == input$log_rows_selected) %>%
      slice(1, n()) %>%
      pull(date_time)
    
    plot %>% dyShading(from = time[1], to = time[2], color = "#C1BEE1") 
    
  } else {
    plot
  }
}

### Split and label events in file ---------------------------------------------

assign_event <- function(fic, val){
  event_count <- c(rep(NA, length(fic)))
  
  event <- 0
  first_count <- T
  second_count <- T
  
  for (i in 1:length(fic)) {
    
    if (fic[i] == 100) {
      if (first_count) {
        event <- event + 1
        first_count <- F
        second_count <- T
      }
      
      event_count[i] = event
      
    } else if (fic[i] - fic[i - 1] != 0 | val[i] != val[i - 1]) {
      
      if(val[i] != val[i - 1]) {second_count <- T}
      
      if (second_count) {
        event <- event + 1
        second_count <- F
        first_count <- T
      }
      
      event_count[i] = event
      
    }
  }
  
  data.frame(event = event_count) %>% fill(event) %>% pull(event)
}

### Processing Normolite column to plot ----------------------------------------

norm_deriv <- function(col){
  index <- which(diff(col) < -1)
  new <- col[1:index[1]]
  
  for (i in 2:(length(index) + 1)){
    value <- sum(col[index[1:i - 1]])
    if (i == (length(index) + 1)){
      aux <- col[(index[i - 1] + 1):length(col)] + value
    } else {
      aux <- col[(index[i - 1] + 1):index[i]] + value
    }
    new <- c(new, aux)
  }
  c(diff(new, lag = 5)/5, rep(NA, 5))
}

### Mode -----------------------------------------------------------------------

mode <- function(x){
  res <- table(x) %>% sort(decreasing = T) %>% names()
  res[1]
}

### reading file ---------------------------------------------------------------

load_file <- function(file){
  read_excel(file) %>% 
    janitor::clean_names() %>% 
    filter(fic_110 != '???') %>%
    mutate(date_time = dmy_hms(date_time), across(2:ncol(.), as.numeric), 
           across(.cols = c(2, 4, 6, 8), .fns = ~ifelse(.x < 0, 0, .x))) %>%
    filter(row_number() >= detect_index(fic_140, \(x) x == 100)) %>% 
    rename(p_set = pressure_sp_history_out) %>% 
    group_by(event = assign_event(fic_140, rswitch_val)) %>%
    mutate(n = pretty_sec(n() * 60)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      temp = case_when(
        tic_300_sp == tic_300_pv ~ "Isotherm",
        tic_300_sp > tic_300_pv ~ "Heating",
        tic_300_sp < tic_300_pv ~ "Cooling down"),
      r1 = if_else(rswitch_val == 1, sum(fi_120, fi_130, fi_140), fi_110),
      r2 = if_else(rswitch_val == 1, fi_110, sum(fi_120, fi_130, fi_140))) %>%
    ungroup() %>%
    group_by(event) %>%
    mutate(temp = mode(temp)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(name = str_glue("R1-{round(r1)} R2-{round(r2)} T-{temp} P-{p_set}")) %>%
    ungroup() 
}

  
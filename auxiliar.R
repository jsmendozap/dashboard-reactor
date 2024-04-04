### Split and label events in file ---------------------------------------------

assign_event <- function(df){
  df <- df %>% 
    select(fic_140, rswitch_val) %>%
    mutate(event = NA)
  
  event <- 0
  first_count <- T
  second_count <- T
  
  for (i in 1:dim(df)[1]) {
    
    if (df[i, "fic_140"] == 100) {
      if (first_count) {
        event <- event + 1
        first_count <- F
        second_count <- T
      }
      
      df[i, "event"] = event
      
    } else if (df[i, "fic_140"] - df[i - 1, "fic_140"] != 0 |
               df[i, "rswitch_val"] != df[i - 1, "rswitch_val"]) {
      
      if(df[i, "rswitch_val"] != df[i - 1, "rswitch_val"]) {second_count <- T}
      
      if (second_count) {
        event <- event + 1
        second_count <- F
        first_count <- T
      }
      
      df[i, "event"] = event
      
    }
  }
  df %>% fill(event) %>% pull(event)
}

### Processing Normolite column to plot ----------------------------------------

norm_deriv <- function(col, lag){
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
  
  diff(new, lag = lag)/lag
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
    select(1:11, 13:17, 25:26) %>%
    mutate(across(.cols = c(2, 4, 6, 8), .fns = ~ifelse(.x < 0, 0, .x))) %>%
    filter(row_number() >= detect_index(fic_140, \(x) x == 100)) %>% 
    rename('p_set' = 12) %>% 
    group_by(event = assign_event(.)) %>%
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
  
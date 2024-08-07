### Mode -----------------------------------------------------------------------

mode <- function(x){
  res <- table(x) %>% sort(decreasing = T) %>% names()
  res[1]
}

### reading file ---------------------------------------------------------------

load_file <- function(file){
  read_excel(file) %>% 
    janitor::clean_names() %>% 
    filter(!grepl("\\?", fic_110)) %>% 
    mutate(date_time = dmy_hms(date_time), across(2:ncol(.), as.numeric), 
           across(.cols = c(2, 4, 6, 8), .fns = ~ifelse(.x < 0, 0, .x))) %>% 
    rename(p_set = pressure_sp_history_out) %>% 
    group_by(event = assign_event(fic_110, fic_120, fic_130, fic_140, rswitch_val, p_set)) %>%
    mutate(n = n(), time_duration = pretty_sec(n() * 60)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      temp = case_when(
        tic_300_sp == tic_300_pv ~ "Isotherm",
        tic_300_sp > tic_300_pv ~ "Heating",
        tic_300_sp < tic_300_pv ~ "Cooling down"),
      r1 = if_else(rswitch_val == 1, sum(fic_120, fic_130, fic_140), fic_110),
      r2 = if_else(rswitch_val == 1, fic_110, sum(fic_120, fic_130, fic_140))) %>%
    ungroup() %>%
    mutate(temp = mode(temp), .by = event) %>%
    rowwise() %>%
    mutate(name = str_glue("R1-{r1} R2-{r2} T-{temp} P-{p_set}")) %>%
    ungroup() 
}

  
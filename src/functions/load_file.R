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
    rename(p_set = pressure_sp_history_out) %>% 
    group_by(event = assign_event(fic_110, fic_120, fic_130, fic_140, rswitch_val)) %>%
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

  
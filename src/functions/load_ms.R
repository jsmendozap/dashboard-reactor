load_ms <- function(path, bd){
  data <- bd %>%
    mutate(row = row_number()) %>%
    reframe(date_time = seq(date_time, as.POSIXct(date_time) + 59, by = '1 sec'),
            .by = row) %>% 
    left_join(bd %>% select(date_time, te_310, te_320),
              by = join_by(date_time)) %>%
    mutate(across(.cols = c(te_310, te_320), .fns = ~na.approx(.x, na.rm = F))) %>%
    fill(te_310, te_320) 
  
  read_delim(path, skip = 377) %>% 
    janitor::clean_names() %>%
    select(time_absolute_date_time, contains("_amu_")) %>%
    mutate(time_absolute_date_time = mdy_hms(time_absolute_date_time)) %>% 
    left_join(data, by = join_by(closest(time_absolute_date_time >= date_time))) %>%
    select(-c(row, date_time)) %>% 
    rename_with(.fn = ~substr(.x, 2, nchar(.)), .cols = contains("_amu_"))
}
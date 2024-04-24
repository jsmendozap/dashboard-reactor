load_ms <- function(path, bd){
  data <- bd %>%
    mutate(row = row_number()) %>%
    reframe(date_time = seq(date_time, as.POSIXct(date_time) + 59, by = '1 sec'),
            .by = row) %>% 
    left_join(bd %>% select(date_time, tic_300_pv), by = join_by(date_time)) %>%
    mutate(tic_300_pv = na.approx(tic_300_pv, na.rm = F)) %>%
    fill(tic_300_pv) 
  
  data <- bd %>%
    summarise(event = mean(event),
              start = date_time[1],
              end = date_time[n()], .by = 'event') %>%
    {left_join(data, ., by = join_by(between(date_time, start, end)))} 
  
  read_delim(path, skip = 377, show_col_types = F) %>% 
    janitor::clean_names() %>%
    select(time_absolute_date_time, contains("_amu_")) %>%
    mutate(time_absolute_date_time = mdy_hms(time_absolute_date_time)) %>% 
    left_join(data, by = join_by(closest(time_absolute_date_time >= date_time))) %>%
    rename_with(.fn = ~substr(.x, 2, nchar(.)), .cols = contains("_amu_")) %>%
    fill(event, .direction = 'up') %>%
    mutate(time = difftime(time_absolute_date_time, start, units = 'mins') %>% as.numeric) %>%
    select(-c(row, date_time, start, end)) %>%
    relocate(time_absolute_date_time, time, event, tic_300_pv)
}

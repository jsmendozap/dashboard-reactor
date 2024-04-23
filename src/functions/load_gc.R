load_gc <- function(path, bd){
  gc <- read_delim(file = path, skip = 12, na = 'n.a.') %>%
    janitor::clean_names() %>%
    rename(injection = 1, inject_time = 2) %>% 
    mutate(inject_time = dmy_hm(inject_time))
  
  bd %>%
    summarise(event = mean(event),
              start = date_time[1],
              end = date_time[n()], .by = 'event') %>% 
    {left_join(gc, ., by = join_by(between(inject_time, start, end)))} %>%
    left_join(select(bd, date_time, tic_300_pv), by = join_by(closest(inject_time >= date_time))) %>%
    fill(event) %>% 
    drop_na(event) %>%
    group_by(event) %>%
    mutate(time = round(inject_time - start)) %>%
    ungroup() %>%
    select(inject_time, event, time, tic_300_pv, !where(is.logical), -c(x3, date_time, start, end)) %>% 
    mutate(across(.cols = 6:ncol(.), ~replace_na(.x, 0)))
}

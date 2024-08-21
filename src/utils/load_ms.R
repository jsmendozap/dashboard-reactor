load_ms <- function(path, bd){
  data <- bd %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::reframe(date_time = seq(date_time, as.POSIXct(date_time) + 59, by = '1 sec'),
                   .by = row) %>%
    dplyr::left_join(bd %>% dplyr::select(date_time, tic_300_pv),
                     by = dplyr::join_by(date_time)) %>%
    dplyr::mutate(tic_300_pv = zoo::na.approx(tic_300_pv, na.rm = F)) %>%
    tidyr::fill(tic_300_pv)

  data <- bd %>%
    dplyr::summarise(event = mean(event),
              start = date_time[1],
              end = date_time[dplyr::n()], .by = 'event') %>%
    {dplyr::left_join(data, ., by = dplyr::join_by(dplyr::between(date_time, start, end)))}

  readr::read_delim(path, skip = 377, show_col_types = F) %>%
    janitor::clean_names() %>%
    dplyr::select(time_absolute_date_time, dplyr::contains("_amu_")) %>%
    dplyr::mutate(time_absolute_date_time = lubridate::mdy_hms(time_absolute_date_time)) %>%
    dplyr::left_join(data, by = dplyr::join_by(closest(time_absolute_date_time >= date_time))) %>%
    dplyr::rename_with(.fn = ~substr(.x, 2, nchar(.)), .cols = dplyr::contains("_amu_")) %>%
    tidyr::fill(event, .direction = 'up') %>%
    dplyr::mutate(time = difftime(time_absolute_date_time, start, units = 'mins') %>% as.numeric) %>%
    dplyr::select(-c(row, date_time, start, end)) %>%
    dplyr::relocate(time_absolute_date_time, time, event, tic_300_pv)
}

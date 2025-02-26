load_gc <- function(path, bd) {
  gc <- readr::read_delim(
    file = path,
    skip = 12,
    na = 'n.a.',
    show_col_types = F,
    name_repair = 'universal_quiet'
  ) %>%
    janitor::clean_names() %>%
    dplyr::rename(injection = 1, inject_time = 2) %>%
    dplyr::mutate(inject_time = lubridate::dmy_hm(inject_time))

  bd %>%
    dplyr::summarise(
      event = mean(event),
      start = date_time[1],
      end = date_time[dplyr::n()],
      .by = 'event'
    ) %>%
    {
      dplyr::left_join(
        gc,
        .,
        by = dplyr::join_by(dplyr::between(inject_time, start, end))
      )
    } %>%
    dplyr::left_join(
      dplyr::select(bd, date_time, tic_300_pv),
      by = dplyr::join_by(closest(inject_time >= date_time))
    ) %>%
    tidyr::fill(event) %>%
    tidyr::drop_na(event) %>%
    dplyr::mutate(
      time = difftime(inject_time, start, units = 'mins') %>% as.numeric
    ) %>%
    dplyr::select(
      inject_time,
      event,
      time,
      tic_300_pv,
      !where(is.logical),
      -c(x3, date_time, start, end)
    ) %>%
    dplyr::mutate(dplyr::across(.cols = 6:ncol(.), ~ tidyr::replace_na(.x, 0)))
}

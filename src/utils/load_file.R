### Mode -----------------------------------------------------------------------

mode <- function(x) {
  res <- table(x) %>% sort(decreasing = T) %>% names()
  res[1]
}

### reading file ---------------------------------------------------------------

load_file <- function(file) {
  openxlsx::readWorkbook(file) %>%
    janitor::clean_names() %>%
    dplyr::filter(!grepl("\\?", fic_110)) %>%
    dplyr::mutate(
      date_time = lubridate::dmy_hms(date_time),
      dplyr::across(2:ncol(.), as.numeric),
      dplyr::across(.cols = c(2, 4, 6, 8), .fns = ~ ifelse(.x < 0, 0, .x))
    ) %>%
    dplyr::rename(p_set = pressure_sp_history_out) %>%
    dplyr::group_by(
      event = assign_event(
        fic_110,
        fic_120,
        fic_130,
        fic_140,
        rswitch_val,
        p_set
      )
    ) %>%
    dplyr::mutate(
      n = dplyr::n(),
      time_duration = prettyunits::pretty_sec(n() * 60)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      temp = dplyr::case_when(
        tic_300_sp == tic_300_pv ~ "Isotherm",
        tic_300_sp > tic_300_pv ~ "Heating",
        tic_300_sp < tic_300_pv ~ "Cooling down"
      ),
      r1 = ifelse(rswitch_val == 1, sum(fic_120, fic_130, fic_140), fic_110),
      r2 = ifelse(rswitch_val == 1, fic_110, sum(fic_120, fic_130, fic_140))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(temp = mode(temp), .by = event) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      name = stringr::str_glue("R1-{r1} R2-{r2} T-{temp} P-{p_set}")
    ) %>%
    dplyr::ungroup()
}

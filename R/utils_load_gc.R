#' load_gc
#'
#' @description Reads a delimited file, cleans column names, processes timestamps, and merges it with the reactor dataset (`bd`) to calculate events and associated metrics.
#'
#' @param path A string. Path to the delimited file to be read. The file is expected to have a specific structure, with data starting after 12 skipped lines.
#' @param bd A dataframe containing reactor information.
#'
#' @details
#' The function performs the following steps:
#' - Reads the file at the specified `path` using `readr::read_delim`, skipping the first 12 lines and treating "n.a." as missing values.
#' - Cleans column names using `janitor::clean_names` and renames the first two columns to `injection` and `inject_time`.
#' - Converts the `inject_time` column to a datetime format using `lubridate::dmy_hm`.
#' - Summarizes the `bd` dataframe to calculate event start and end times.
#' - Merges the summarized `bd` with the cleaned file data (`gc`) based on time intervals and closest matches.
#' - Fills missing event values, drops rows with missing events, and calculates the time difference between injection time and event start.
#' - Selects and cleans relevant columns, replacing missing values in numeric columns with 0.
#'
#' @return A processed dataframe with the following columns:
#' - `inject_time`: The timestamp of the injection.
#' - `event`: The event identifier.
#' - `time`: Time difference (in minutes) between injection time and event start.
#' - `tic_300_pv`: A numeric column from the input `bd`.
#' - Numeric columns with compounds setted in the GC.
#'
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom dplyr rename mutate summarise left_join select n across between join_by
#' @importFrom tidyr fill drop_na replace_na
#' @importFrom lubridate dmy_hm
#'
#' @noRd

load_gc <- function(path, bd) {
  gc <- read_delim(
    file = path,
    skip = 12,
    na = 'n.a.',
    show_col_types = F,
    name_repair = 'universal_quiet'
  ) %>%
    clean_names() %>%
    rename(injection = 1, inject_time = 2) %>%
    mutate(inject_time = dmy_hm(inject_time))

  bd %>%
    summarise(
      event = mean(event),
      start = date_time[1],
      end = date_time[n()],
      .by = 'event'
    ) %>%
    {
      left_join(
        gc,
        .,
        by = join_by(between(inject_time, start, end))
      )
    } %>%
    left_join(
      select(bd, date_time, tic_300_pv),
      by = join_by(closest(inject_time >= date_time))
    ) %>%
    fill(event) %>%
    drop_na(event) %>%
    mutate(
      time = difftime(inject_time, start, units = 'mins') %>% as.numeric
    ) %>%
    select(
      inject_time,
      event,
      time,
      tic_300_pv,
      !where(is.logical),
      -c(x3, date_time, start, end)
    ) %>%
    mutate(across(.cols = 6:ncol(.), ~ replace_na(.x, 0)))
}

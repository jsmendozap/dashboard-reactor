#' load_ms
#'
#' @description Reads an MS data file, processes it, and merges it with an
#' experimental database (`bd`) to associate mass spectrometry readings with events.
#'
#' @param path Character. Path to the MS data file.
#' @param bd A dataframe containing reactor information.
#'
#' @details
#' The function performs the following steps:
#' - Expands the `bd` dataframe to include one-second intervals for each row.
#' - Interpolates missing values in the `tic_300_pv` column using linear approximation.
#' - Summarizes the `bd` dataframe to calculate event start and end times.
#' - Reads the MS data file, cleans column names, and selects relevant columns.
#' - Converts the `time_absolute_date_time` column to a datetime format.
#' - Merges the MS data with the expanded `bd` dataframe based on time intervals.
#' - Fills missing event values and calculates the time difference between MS readings and event start times.
#' - Reorganizes and cleans the final dataframe for output.
#'
#' @return A dataframe containing the processed MS data with merged event details, including:
#' - `time_absolute_date_time`: The timestamp of the MS reading.
#' - `time`: Time difference (in minutes) between the MS reading and the event start.
#' - `event`: The event identifier.
#' - `tic_300_pv`: Interpolated reactor data.
#' - Other columns from the MS data file, cleaned and processed.
#'
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate reframe left_join summarise select contains rename_with relocate row_number n between join_by
#' @importFrom tidyr fill
#' @importFrom lubridate mdy_hms
#' @importFrom zoo na.approx
#'
#' @export

load_ms <- function(path, bd) {
  data <- bd %>%
    mutate(row = row_number()) %>%
    reframe(
      date_time = seq(date_time, as.POSIXct(date_time) + 59, by = '1 sec'),
      .by = row
    ) %>%
    left_join(
      bd %>% select(date_time, tic_300_pv),
      by = join_by(date_time)
    ) %>%
    mutate(tic_300_pv = na.approx(tic_300_pv, na.rm = F)) %>%
    fill(tic_300_pv)

  data <- bd %>%
    summarise(
      event = mean(event),
      start = date_time[1],
      end = date_time[n()],
      .by = 'event'
    ) %>%
    {
      left_join(
        data,
        .,
        by = join_by(between(date_time, start, end))
      )
    }

  read_delim(path, skip = 377, show_col_types = F) %>%
    clean_names() %>%
    select(time_absolute_date_time, contains("_amu_")) %>%
    mutate(
      time_absolute_date_time = mdy_hms(time_absolute_date_time)
    ) %>%
    left_join(
      data,
      by = join_by(closest(time_absolute_date_time >= date_time))
    ) %>%
    rename_with(
      .fn = ~ substr(.x, 2, nchar(.)),
      .cols = contains("_amu_")
    ) %>%
    fill(event, .direction = 'up') %>%
    mutate(
      time = difftime(time_absolute_date_time, start, units = 'mins') %>%
        as.numeric
    ) %>%
    select(-c(row, date_time, start, end)) %>%
    relocate(time_absolute_date_time, time, event, tic_300_pv)
}

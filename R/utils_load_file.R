#' load_file
#'
#' @description Reads an Excel file, cleans column names, filters data, processes events, and calculates derived metrics for reactor operations.
#'
#' @param file A string. Path to the Excel file to be read.
#'
#' @details
#' The function performs the following steps:
#' - Reads the Excel file using `openxlsx::readWorkbook`.
#' - Cleans column names using `janitor::clean_names`.
#' - Filters out rows where the `fic_110` column contains a "?" character.
#' - Converts the `date_time` column to a datetime format using `lubridate::dmy_hms`.
#' - Converts numeric columns to numeric types and replaces negative values in specific columns with 0.
#' - Renames the `pressure_sp_history_out` column to `p_set`.
#' - Groups data by events, which are assigned using the `assign_event` function.
#' - Calculates the event duration in a human-readable format (`time_duration`).
#' - Derives additional metrics such as:
#'   - `temp`: Describes the temperature state as "Isotherm", "Heating", or "Cooling down".
#'   - `r1` and `r2`: Reactor flow rates based on the value of `rswitch_val`.
#' - Assigns a descriptive name to each event based on calculated metrics.
#'
#' @return A processed dataframe with the following columns:
#' - `date_time`: The timestamp of each record.
#' - `event`: The event identifier.
#' - `n`: The number of rows associated with each event.
#' - `time_duration`: The duration of each event in a human-readable format.
#' - `temp`: The temperature state ("Isotherm", "Heating", or "Cooling down").
#' - `r1`: Reactor 1 flow rate.
#' - `r2`: Reactor 2 flow rate.
#' - `name`: A descriptive name for each event.
#' - Other columns from the input file, cleaned and processed as needed.
#'
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom openxlsx readWorkbook
#' @importFrom dplyr filter mutate across rename group_by n rowwise ungroup case_when
#' @importFrom lubridate dmy_hms
#'
#' @noRd

### Mode -----------------------------------------------------------------------

mode <- function(x) {
  res <- table(x) %>% sort(decreasing = T) %>% names()
  res[1]
}

### Reading file ---------------------------------------------------------------

load_file <- function(file) {
  readWorkbook(file) %>%
    clean_names() %>%
    filter(!grepl("\\?", fic_110)) %>%
    mutate(
      date_time = dmy_hms(date_time),
      across(2:ncol(.), as.numeric),
      across(.cols = c(2, 4, 6, 8), .fns = ~ ifelse(.x < 0, 0, .x))
    ) %>%
    rename(p_set = pressure_sp_history_out) %>%
    group_by(
      event = assign_event(
        fic_110,
        fic_120,
        fic_130,
        fic_140,
        rswitch_val,
        p_set
      )
    ) %>%
    mutate(
      n = n(),
      time_duration = prettyunits::pretty_sec(n() * 60)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      temp = case_when(
        tic_300_sp == tic_300_pv ~ "Isotherm",
        tic_300_sp > tic_300_pv ~ "Heating",
        tic_300_sp < tic_300_pv ~ "Cooling down"
      ),
      r1 = ifelse(rswitch_val == 1, sum(fic_120, fic_130, fic_140), fic_110),
      r2 = ifelse(rswitch_val == 1, fic_110, sum(fic_120, fic_130, fic_140))
    ) %>%
    ungroup() %>%
    mutate(temp = mode(temp), .by = event) %>%
    rowwise() %>%
    mutate(
      name = stringr::str_glue("R1-{r1} R2-{r2} T-{temp} P-{p_set}")
    ) %>%
    ungroup()
}

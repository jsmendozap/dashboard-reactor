#' assign_event
#'
#' @description Splits and labels events in a dataset based on changes in input variables. 
#' This function assigns a unique event identifier to each segment of data where specific 
#' conditions change, such as variations in gas composition, valve states, or pressure.
#'
#' @param air A numeric vector representing the air composition.
#' @param co2 A numeric vector representing the CO2 composition.
#' @param ar A numeric vector representing the argon (Ar) composition.
#' @param n2 A numeric vector representing the nitrogen (N2) composition.
#' @param val A numeric vector representing the valve state.
#' @param p A numeric vector representing the pressure.
#'
#' @details
#' The function iterates through the input vectors and assigns a new event identifier 
#' whenever a change is detected in one or more of the following:
#' - Gas composition (`air`, `co2`, `ar`, `n2`).
#' - Valve state (`val`), with a special condition that `n2` must not be 100.
#' - Pressure (`p`).
#' 
#' The first row is always assigned to the first event. Missing event values are filled 
#' using the `tidyr::fill` function to ensure continuity.
#'
#' @return A numeric vector of the same length as the input vectors, where each element 
#' corresponds to the event identifier for that row.
#'
#' @importFrom tidyr fill
#' @importFrom dplyr pull
#'
#' @examples
#' air <- c(0, 0, 1, 1, 0)
#' co2 <- c(0, 0, 0, 1, 1)
#' ar <- c(0, 0, 0, 0, 0)
#' n2 <- c(100, 100, 100, 50, 50)
#' val <- c(1, 1, 2, 2, 2)
#' p <- c(1, 1, 1, 2, 2)
#' assign_event(air, co2, ar, n2, val, p)
#'
#' @noRd

assign_event <- function(air, co2, ar, n2, val, p) {
  event_count <- rep(NA, length(n2))
  event <- 0

  for (i in 1:length(n2)) {
    if (i == 1) {
      event <- event + 1
      event_count[i] = event
    } else if (
      any(
        n2[i] != n2[i - 1],
        air[i] != air[i - 1],
        co2[i] != co2[i - 1],
        ar[i] != ar[i - 1]
      )
    ) {
      event <- event + 1
      event_count[i] = event
    } else if (val[i] != val[i - 1] && n2[i] != 100) {
      event <- event + 1
      event_count[i] = event
    } else if (p[i] != p[i - 1]) {
      event <- event + 1
      event_count[i] = event
    }
  }

  data.frame(event = event_count) %>% 
    fill(event) %>% 
    pull(event)
}

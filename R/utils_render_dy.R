#' Render Dygraph
#'
#' @description A utility function to create and customize a `dygraph` plot for time-series data.
#'
#' @param bd A dataframe. The dataset containing the time-series data to be plotted.
#' @param selected A character vector. The names of the columns to be plotted in the `dygraph`.
#' @param legend A string. The ID of the HTML element where the legend will be displayed.
#'
#' @details
#' This function generates an interactive `dygraph` plot with the following features:
#' - Plots the selected columns from the input dataframe (`bd`).
#' - Adds a range selector for zooming into specific time intervals.
#' - Supports dual-axis plotting if two columns are selected.
#' - Applies custom CSS styling using the `dygraph.css` file.
#' - Displays a legend in the specified HTML element (`legend`).
#' - Highlights specific time intervals based on the selected event in a `reactable` table.
#'
#' The function suppresses warnings during execution to ensure a clean output.
#'
#' @return A `dygraph` object with the specified configurations.
#'
#' @importFrom dygraphs dygraph dySeries dyRangeSelector dyOptions dyLegend dyShading
#' @importFrom dplyr select all_of slice_head filter row_number pull slice n
#' @importFrom reactable getReactableState
#' @noRd

renderdy <- function(bd, selected, legend) {
  suppressWarnings({
    plot <- dygraph(
      bd %>% select(1, all_of(selected)),
      group = 'log'
    ) %>%
      dySeries(selected[1]) %>%
      dyRangeSelector() %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dyLegend(labelsDiv = legend)

    if (length(selected) >= 2) {
      plot <- plot %>% dySeries(selected[2], axis = "y2")
    }

    if (!is.null(getReactableState('log', 'selected'))) {
      selected_event <- bd %>%
        slice_head(n = 1, by = event) %>%
        filter(row_number() == getReactableState('log', 'selected')) %>%
        pull(event)

      time <- bd %>%
        filter(event == selected_event) %>%
        slice(1, n()) %>%
        pull(date_time)

      plot %>%
        dyShading(from = time[1], to = time[2], color = "#C1BEE1")
    } else {
      plot
    }
  })
}

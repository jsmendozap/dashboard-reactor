#' Reactor Values
#'
#' @description A utility function to display a value box with reactor flow information based on the selected event in a `reactable` table.
#'
#' @param data A dataframe. The dataset containing reactor information, including flow rates and events.
#' @param fi A string. The name of the column in `data` that contains the flow rate values.
#' @param comp A string. The name of the component (e.g., "Reactor Flow") to be displayed in the value box.
#'
#' @details
#' This function retrieves the flow rate value for the selected event in the `reactable` table with ID `log_summary`.
#' It then displays the value in a `bs4Dash::valueBox` with a custom style.
#'
#' - The selected event is determined using `reactable::getReactableState`.
#' - The flow rate value is extracted from the specified column (`fi`) in the `data` dataframe.
#' - The value box displays the component name (`comp`) and the flow rate value with units ("mL/min").
#'
#' @return A `bs4Dash::valueBox` object displaying the reactor flow information.
#'
#' @importFrom shiny req
#' @importFrom reactable getReactableState
#' @importFrom dplyr slice_head pull
#' @importFrom bs4Dash valueBox
#' @importFrom htmltools span
#' @noRd

reactor_values <- function(data, fi, comp) {
  shiny::req(reactable::getReactableState('log', 'selected'))

  sel <- reactable::getReactableState('log', 'selected')

  value <- data %>%
    dplyr::slice_head(n = 1, by = event) %>%
    dplyr::pull(fi)

  bs4Dash::valueBox(
    htmltools::span('\n', comp, style = 'font-size: 18px; font-weight: 600'),
    htmltools::span(
      value[sel],
      " mL/min",
      style = 'font-size: 16px; font-weight: 500'
    )
  )
}

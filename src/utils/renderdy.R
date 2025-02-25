### Render dygraph -------------------------------------------------------------

renderdy <- function(bd, selected, legend) {
  suppressWarnings({
    plot <- dygraphs::dygraph(
      bd %>% dplyr::select(1, dplyr::all_of(selected)),
      group = 'log'
    ) %>%
      dygraphs::dySeries(selected[1]) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyOptions(useDataTimezone = TRUE) %>%
      dygraphs::dyCSS("dygraph.css") %>%
      dygraphs::dyLegend(labelsDiv = legend)

    if (length(selected) >= 2) {
      plot <- plot %>% dygraphs::dySeries(selected[2], axis = "y2")
    }

    if (!is.null(reactable::getReactableState('log', 'selected'))) {
      selected <- dplyr::slice_head(bd, n = 1, by = event) %>%
        dplyr::filter(
          dplyr::row_number() == reactable::getReactableState('log', 'selected')
        ) %>%
        dplyr::pull(event)

      time <- bd %>%
        dplyr::filter(event == selected) %>%
        dplyr::slice(1, dplyr::n()) %>%
        dplyr::pull(date_time)

      plot %>%
        dygraphs::dyShading(from = time[1], to = time[2], color = "#C1BEE1")
    } else {
      plot
    }
  })
}

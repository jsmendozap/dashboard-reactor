reactor_values <- function(data, fi, comp){
  shiny::req(reactable::getReactableState('log', 'selected'))

  sel <- reactable::getReactableState('log', 'selected')

  value <- data %>%
    dplyr::slice_head(n = 1, by = event) %>%
    dplyr::pull(fi)

  bs4Dash::valueBox(htmltools::span('\n', comp,
                                    style = 'font-size: 18px; font-weight: 600'),
                    htmltools::span(value[sel], " mL/min",
                                    style = 'font-size: 16px; font-weight: 500'))
}

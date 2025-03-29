#' Custom Reactable Table
#'
#' @description A utility function to create a customized `reactable` table with predefined styles and behaviors.
#'
#' @param bd A dataframe. The data to be displayed in the `reactable` table.
#' @param ... Additional arguments passed to `reactable::reactable` for further customization.
#'
#' @details
#' This function simplifies the creation of `reactable` tables by applying default styles and configurations, including:
#' - Center-aligned column content.
#' - A light gray header background (`#f7f7f8`).
#' - A default page size of 10 rows.
#' - Row selection on click.
#' - Bordered, highlighted, and resizable columns.
#' - Sortable columns.
#'
#' Users can override these defaults or add additional configurations using the `...` argument.
#'
#' @return A `reactable` table object with the specified data and configurations.
#'
#' @importFrom reactable reactable colDef
#' @noRd

custom_reactable <- function(bd, ...) {
  reactable(
    bd,
    defaultColDef = colDef(
      align = "center",
      #headerStyle = list(background = "#f7f7f8")
    ),
    defaultPageSize = 10,
    onClick = 'select',
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    showSortable = TRUE,
    ...
  )
}

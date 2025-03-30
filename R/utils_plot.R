#' Custom Plot Utility
#'
#' @description A utility function to create customizable `ggplot2` plots with various options for points, lines, areas, horizontal lines, boxplots, and facets.
#'
#' @param data A dataframe. The dataset to be used for plotting.
#' @param x The column to be used for the x-axis.
#' @param y The column to be used for the y-axis.
#' @param points Logical. Whether to include points in the plot (`geom_point`).
#' @param lines Logical. Whether to include lines in the plot (`geom_line`).
#' @param hline Logical. Whether to include horizontal lines in the plot (`geom_hline`).
#' @param area Logical. Whether to include an area plot (`geom_area`).
#' @param boxp Logical. Whether to include a boxplot (`geom_boxplot`).
#' @param facet A string. The column to be used for faceting (`facet_wrap`).
#' @param xlab A string. The label for the x-axis.
#' @param ylab A string. The label for the y-axis.
#' @param title A string. The title of the plot.
#' @param scale_y A list. Additional arguments for `scale_y_continuous`.
#' @param custom_color Logical. Whether to apply custom colors based on a predefined `compounds` dataset.
#' @param args A list. Additional arguments for specific geoms (e.g., `points`, `lines`, `area`, `hline`, `boxp`, `facet`).
#' @param ... Additional aesthetics passed to `ggplot2::aes`.
#'
#' @details
#' This function provides a flexible way to create `ggplot2` plots with multiple layers and customizations. Users can specify which geoms to include and provide additional arguments for fine-tuning.
#'
#' @return A `ggplot2` object representing the customized plot.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_area geom_hline geom_boxplot facet_wrap scale_fill_manual scale_y_continuous labs theme_bw
#' @importFrom purrr exec
#' @importFrom dplyr %>%
#' @noRd

plot <- function(
  data,
  x,
  y,
  points = FALSE,
  lines = FALSE,
  hline = FALSE,
  area = FALSE,
  boxp = FALSE,
  facet = NULL,
  xlab,
  ylab,
  title = NULL,
  scale_y = NULL,
  custom_color = FALSE,
  args = list(),
  ...
) {
  p <- ggplot(
    data,
    aes(x = {{ x }}, y = {{ y }}, ...)
  )

  p <- p +
    list(
      if (points) {
        if ("points" %in% names(args)) {
          do.call(geom_point, args$points)
        } else {
          geom_point()
        }
      },
      if (lines) {
        if ("lines" %in% names(args)) {
          do.call(geom_line, args$lines)
        } else {
          geom_line()
        }
      },
      if (area) {
        if ("area" %in% names(args)) {
          do.call(geom_area, args$area)
        } else {
          geom_area()
        }
      },
      if (hline) {
        if ("hline" %in% names(args)) {
          do.call(geom_hline, args$hline)
        } else {
          geom_hline()
        }
      },
      if (boxp) {
        if ("boxp" %in% names(args)) {
          do.call(geom_boxplot, args$boxp)
        } else {
          geom_boxplot()
        }
      },
      if (!is.null(facet)) {
        if ("facet" %in% names(args)) {
          exec(.f = facet_wrap, ~ .data[[facet]], !!!args$facet)
        } else {
          facet_wrap(~ .data[[facet]])
        }
      },
      if (custom_color) {
        cmp <- app_state$setting() %>%
          dplyr::select(compound, color) %>%
          {
            setNames(.$color, .$compound)
          }
        scale_fill_manual(
          values = cmp[names(cmp) %in% unique(data$Compound)]
        )
      },
      if (!is.null(scale_y)) do.call(scale_y_continuous, scale_y)
    )

  p <- p +
    labs(x = xlab, y = ylab, title = title) +
    theme_bw()

  return(p)
}

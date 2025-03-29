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

compounds <- tibble::tribble(
  ~name,
  ~formula,
  ~weight,
  ~carbon,
  ~hydrogen,
  ~oxygen,
  ~color,
  "Hydrogen",
  "H2",
  2,
  0L,
  2L,
  0L,
  "orange",
  "Carbon monoxide",
  "CO",
  28.01,
  1L,
  0L,
  1L,
  "lightgreen",
  "Carbon dioxide",
  "CO2",
  44.01,
  1L,
  0L,
  2L,
  "red",
  "Methane",
  "CH4",
  16.04,
  1L,
  4L,
  0L,
  "blue",
  "Ethane",
  "C2H6",
  30.07,
  2L,
  6L,
  0L,
  "purple",
  "Ethylene",
  "C2H4",
  28.05,
  2L,
  4L,
  0L,
  "yellow",
  "Propane",
  "C3H8",
  44.097,
  3L,
  8L,
  0L,
  "gray",
  "Propylene",
  "C3H6",
  42.081,
  3L,
  6L,
  0L,
  "royalblue",
  "n-butane",
  "n-C4H10",
  58.012,
  4L,
  10L,
  0L,
  "deeppink",
  "i-butane",
  "iso-C4H10",
  58.012,
  4L,
  10,
  0L,
  "mediumseagreen",
  "Cis-2-butene",
  "c-C4H8",
  56.1,
  4L,
  8L,
  0L,
  "darkviolet",
  "t-2-butene",
  "t-C4H8",
  56.1,
  4L,
  8L,
  0L,
  "chartreuse",
  "Nitrogen",
  "N2",
  28.0134,
  0L,
  0L,
  0L,
  "dodgerblue",
  "Argon",
  "Ar",
  39.948,
  0L,
  0L,
  0L,
  "turquoise",
  "Water",
  "H2O",
  18.015,
  0L,
  2L,
  1L,
  "olive"
)

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
        cmp <- compounds %>%
          dplyr::select(name, color) %>%
          {
            setNames(.$color, .$name)
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

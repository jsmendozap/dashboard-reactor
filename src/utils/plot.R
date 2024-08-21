plot <- function(
  data, 
  x,
  y,
  points = F,
  lines = F,
  hline = F,
  area = F,
  facet = NULL,
  xlab,
  ylab,
  title = NULL,
  scale_y = NULL,
  args = list(),
  ...
) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, ...))

  p <- p + 
    list(
      if(points){
        if ("points" %in% names(args)) {do.call(ggplot2::geom_point, args$points)} else {ggplot2::geom_point()}
      },
      if(lines){
        if ("lines" %in% names(args)) {do.call(ggplot2::geom_line, args$lines)} else {ggplot2::geom_line()}
      },
      if(area){
        if ("area" %in% names(args)) {do.call(ggplot2::geom_area, args$area)} else {ggplot2::geom_area()}
      },
      if(hline){
        if ("hline" %in% names(args)) {do.call(ggplot2::geom_hline, args$hline)} else {ggplot2::geom_hline()}
      },
      if(!is.null(facet)){ 
        if ("facet" %in% names(args)) {
          purrr::exec(.f = ggplot2::facet_wrap, ~.data[[facet]], !!!args$facet)
        } else {
          ggplot2::facet_wrap(~ .data[[facet]])
        }
      },
      if(!is.null(scale_y)) do.call(ggplot2::scale_y_continuous, scale_y)
    )
    
    p <- p + ggplot2::labs(x = xlab, y = ylab, title = title) + ggplot2::theme_bw()
    
    return(p)
}

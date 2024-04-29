conditional_color <- function(value) {
  if (value < 0.9 || value >= 1.1) {
    color <- "red"
  } else {
    color <- "lightgreen"
  }
  
  list(color = color, fontWeight = 'bold')
}
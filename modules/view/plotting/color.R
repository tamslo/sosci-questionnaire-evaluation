getColor <- function(index, alternativeOrder = FALSE, add_alpha = TRUE) {
  # Colorblind-friendly pallette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # #a-colorblind-friendly-palette
  # colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  colors <- c("#D55E00", "#E69F00", "#F0E442", "#56B4E9", "#0072B2")
  if (alternativeOrder) {
    colors <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00", "#F0E442")
  }
  if (index > length(colors)) {
    hexColor <- "#999999"
    alpha <- 1 - ((index %% length(colors) + 1) / length(colors))
  } else {
    hexColor <- colors[index]
    alpha <- 1
  }
  if (add_alpha) {
    rgbColor <- col2rgb(hexColor)
    color <- rgb(rgbColor["red", 1]/255, rgbColor["green", 1]/255, rgbColor["blue", 1]/255, alpha = alpha)
  } else {
    color <- hexColor
  }
  return(color)
}
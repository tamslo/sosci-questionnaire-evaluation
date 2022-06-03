renderBarplot <- function(plotParams) {
  counts <- plotParams[["counts"]]
  labels <- plotParams[["labels"]]
  ylim <- plotParams[["ylim"]]
  colors <- plotParams[["colors"]]
  barplot(counts, names.arg = labels, border = NA, ylim = range(pretty(c(0,ylim))))
  abline(h = 1:ylim, col = "grey", lty = "dotted")
  barplot(counts, add = TRUE, col = colors, border = NA)
}
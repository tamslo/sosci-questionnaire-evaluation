renderComparisonBarplot <- function(plotParams) {
  answers <- plotParams[["answers"]]
  colors <- plotParams[["colors"]]
  labels <- plotParams[["labels"]]
  title <- plotParams[["title"]]
  maxLabelLength <- max(nchar(labels))
  legendSpaceFactor <- ceiling(maxLabelLength / 50)
  maxChars <- (25 - 4 * (legendSpaceFactor - 1)) * legendSpaceFactor
  labels <- lapply(labels, function(label) { breakText(label, maxChars = maxChars) })

  answers <- as.matrix(as.data.frame(answers))
  maxRange <- 100
  par(oma=c(0, 0, 0, 9.5 * legendSpaceFactor))
  barplot(answers, border = NA, xlim = range(pretty(c(0,maxRange))), sub = "% of answers per questionnaire", horiz = TRUE)
  abline(v = 0:maxRange, col = "grey", lty = "dotted")
  abline(v = seq(0, maxRange, by = 10), col = "grey", lty = "solid")
  barplot(answers, add = TRUE, col = colors, border = NA, horiz = TRUE)
  legend(par("usr")[2] + 0.05, par("usr")[4], xpd = NA, bty = "n", legend = labels, fill = colors)
}
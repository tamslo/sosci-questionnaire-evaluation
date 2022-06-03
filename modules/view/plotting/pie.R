source("modules/view/plotting/label.R", local = TRUE)

renderPie <- function(plotParams) {
  counts <- plotParams[["counts"]]
  labels <- plotParams[["labels"]]
  if ("breakPosition" %in% names(plotParams)) {
    breakPosition <- plotParams[["breakPosition"]]
  } else {
    breakPosition <- 50
  }
  adaptedLabels <- c()
  for (index in 1:length(labels)) {
    label <- labels[index]
    count <- counts[index]
    label <- replaceLabel(label)
    label <- appendCount(label, count)
    label <- breakText(label, breakPosition)
    adaptedLabels <- c(adaptedLabels, label)
  }
  colors <- plotParams[["colors"]]
  sub <- paste("N = ", sum(counts))
  if ("description" %in% names(plotParams)) {
    description <- plotParams[["description"]]
    sub <- paste(sub, description, sep = "\n")
  }
  title <- NULL
  if ("title" %in% names(plotParams)) {
    title <- plotParams[["title"]]
  }
  pie(counts, labels = adaptedLabels, col = colors, sub = sub, main = title, border=NA)
}
source("modules/config.R", local = TRUE)

replaceLabel <- function(label) {
  replacements <- getFromConfig(c("replacements", "labels"))
  return(applyReplacement(replacements, label))
}

attachValueToLabel <- function(value, label, responses) {
  return(paste("[", normalizeValue(value, responses), "] ", label, sep = ""))
}

appendCount <- function(label, count) {
  return(paste(label, " (", count, ")", sep = ""))
}

breakTextOnce <- function(name, textLimits, lineBreaks) {
  name <- str_trim(str_replace_all(name, "\n", " "))
  if (nchar(name) > lineBreaks[1]) {
    spaces <- unlist(gregexpr(pattern = " ", name))
    break_position = lineBreaks[min(which(textLimits >= nchar(name), arr.ind = TRUE))]
    # From https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html
    break_position <- min(spaces[which(abs(spaces - break_position) == min(abs(spaces - break_position)))])
    name <- paste(substring(name, 1, break_position - 1), substring(name, break_position + 1), sep = "\n")
  } else {
    # Add single space to align labels top
    name <- paste(name, "\n", sep = "")
  }
  return(name)
}

breakText <- function(name, maxChars) {
  name <- str_trim(str_replace_all(name, "\n", " "))
  if (nchar(name) > maxChars) {
    parts <- unlist(strsplit(name, "\\s+"))
    maxPartLength <- max(nchar(parts))
    maxChars <- max(maxChars, maxPartLength)
    partIndex <- 1
    lines <- c()
    currentLine <- ""
    while (partIndex <= length(parts)) {
      nextLine <- paste(currentLine, parts[partIndex])
      if (nchar(nextLine) <= maxChars) {
        partIndex <- partIndex + 1
        currentLine <- nextLine
      } else {
        lines <- c(lines, currentLine)
        currentLine <- ""
      }
    }
    lines <- c(lines, currentLine)
    name <- str_trim(paste(lines, collapse = "\n"))
  }
  return(name)
}
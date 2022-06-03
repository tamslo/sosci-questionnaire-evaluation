renderSignificanceTable <- function(questionnaireQuestionList, filterParticipants) {
  alpha = 0.05
  comparisons <- testComparisonSignificances(questionnaireQuestionList, alpha, filterParticipants)
  pValues <- c("p-value")
  for (comparison in names(comparisons)) {
    pValue <- round(comparisons[[comparison]][["pValue"]], digits = 2)
    if (is.nan(pValue)) {
      pValue <- "-"
    } else if (pValue <= alpha) {
      pValue <- paste0("<span style='color:green;'>", pValue, "</span>")
    } else if (pValue > alpha) {
      pValue <- paste0("<span style='color:red;'>", pValue, "</span>")
    }
    pValues <- c(pValues, pValue)
  }

  summaryHeader <- c("", names(comparisons))
  summaryContent <- list(pValues)
  return(htmlTable(summaryHeader, summaryContent))
}

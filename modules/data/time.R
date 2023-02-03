getTime <- function(questionnaire) {
  toMinutes <- function(seconds) {
    return(round(seconds/60))
  }
  
  resultData <- getResultData()
  questionnaireData <- resultData[which(resultData$QUESTNNR == questionnaire),]
  seconds <- questionnaireData$TIME_SUM
  if (all(seconds == 0)) {
    timeColumns <- colnames(questionnaireData)[
      which(startsWith(colnames(questionnaireData), "TIME") & !startsWith(colnames(questionnaireData), "TIME_"))]
    seconds <- c()
    for (rowIndex in 1:nrow(questionnaireData)) {
      timeData <- questionnaireData[rowIndex, timeColumns]
      seconds <- c(seconds, sum(timeData[which(!is.na(timeData))]))
    }
  }
  
  return(list(
    "mean" = toMinutes(mean(seconds)),
    "median" = toMinutes(median(seconds)),
    "min" = toMinutes(min(seconds)),
    "max" = toMinutes(max(seconds))
  ))
}
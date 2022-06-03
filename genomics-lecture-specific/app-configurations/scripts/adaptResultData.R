library(stringr)

adaptResultData <- function(resultData) {
  resultData <- mergeDataUsageResults(resultData)
  resultData <- filterTumQ1Time(resultData)
  return(resultData)
}

mergeDataUsageResults <- function(resultData) {
  questionIds <- Filter(function(name) { startsWith(name, "DF02_") }, names(resultData))
  for (row in 1:nrow(resultData)) {
    if (resultData$QUESTNNR[row] == "Q3") {
      for (questionId in questionIds) {
        otherQuestionId = str_replace(questionId, "DF02", "DF03")
        resultData[row, questionId] <- rbind(resultData[row, otherQuestionId])
        resultData[row, otherQuestionId] <- NA
      }
    }
  }
  return(resultData)
}

filterTumQ1Time <- function(resultData) {
  return(resultData[which(resultData$QUESTNNR != "TUM_Q1" | resultData$STARTED >= "2020-11-18"),])
}
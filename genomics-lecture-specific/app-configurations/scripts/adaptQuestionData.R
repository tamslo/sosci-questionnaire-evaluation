adaptQuestionData <- function(questionData) {
  questionData <- generalizeDataUsageQuestion(questionData)
  return(questionData)
}

generalizeDataUsageQuestion <- function(questionData) {
  for (row in 1:nrow(questionData)) {
    question <- questionData[row,]
    if (question$VAR == "DF02_01") {
      questionData$LABEL[row] <- "[Agreement] Q1/2: I think analyzing my own genomic data as part of the AYPG course is useful"
    }
  }
  return(questionData)
}

shortenRegulationQuestion <- function(questionData) {
  for (row in 1:nrow(questionData)) {
    question <- questionData[row,]
    if (startsWith(question$VAR, "RS02")) {
      questionData$QUESTION[row] <- "In light of the GenDG, please share your thoughts on the following:"
    }
  }
  return(questionData)
}


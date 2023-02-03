source("modules/data/load.R", local = TRUE)

getQuestionIds <- function(resultData) {
  questionIdPositions <- which(!grepl("^TIME", names(resultData)) & grepl("[[:digit:]]$", names(resultData)))
  questionIds <- names(resultData)[questionIdPositions]
  return(questionIds)
}

getQuestionTitle <- function(question) {
  questionTitle <- question$text
  if (!is.na(question$option)) {
    questionTitle <- paste(questionTitle, "Option:", question$option)
  }
  return(questionTitle)
}

getQuestions <- function() {
  getOption <- function(questionData, questionId) {
    label <- questionData[questionData$VAR == questionId,]$LABEL
    option <- unlist(regmatches(label, regexpr(": ", label), invert = TRUE))[2]
    return(option)
  }
  
  questionData <- getQuestionData()
  resultData <- getResultData()
  questionIds <- getQuestionIds(resultData)
  
  id <- questionIds
  type <- c()
  page <- c()
  base <- c()
  text <- c()
  option <- c()
  for (questionId in questionIds) {
    question <- questionData[questionData$VAR == questionId,]
    type <- c(type, question$INPUT)
    page <- c(page, substring(questionId, 1, 2))
    base <- c(base, unlist(strsplit(questionId, "_", fixed = TRUE))[1])
    text <- c(text, question$QUESTION)
    option <- c(option, getOption(questionData, questionId))
  }

  return(data.frame(id, type, page, base, text, option))
}
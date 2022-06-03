renderCheckboxQuestion <- function(questionParts, questionnaire, filterParticipants) {
  replacements <- getFromConfig(c("replacements", "titles"))
  counts <- c()
  for (row in 1:nrow(questionParts)) {
    questionPart <- questionParts[row,]
    if (questionPart$type == "CHECKBOX") {
      responses <- getResponses(questionPart, questionnaire, filterParticipants)
      optionCount <- responses[responses$value == 2,]$count
      counts <- c(counts, optionCount)
    } else {
      counts <- c(counts, -1)
    }
  }
  orderedQuestionParts <- questionParts[order(counts, decreasing = TRUE),]
  contentParts <- list()
  currentContent <- 0
  for (row in 1:nrow(orderedQuestionParts)) {
    questionPart <- orderedQuestionParts[row,]
    if (questionPart$type == "CHECKBOX") {
      responses <- getResponses(questionPart, questionnaire, filterParticipants)
      optionCount <- responses[responses$value == 2,]$count
      if (optionCount > 0) {
        currentContent <- currentContent + 1
        optionText <- applyReplacement(replacements, questionPart$option)
        if (endsWith(optionText, ":")) {
          optionText <- str_sub(optionText, end=-2) 
        }
        contentParts[[currentContent]] <- tags$li(paste(optionText, " (", optionCount, ")", sep = ""))
      }
    }
    if (questionPart$type == "OPEN") {
      currentContent <- currentContent + 1
      contentParts[[currentContent]] <- getOpenContent(questionPart, questionnaire, filterParticipants)
    }
  }
  return(tagList(tags$br(), do.call(tags$ul, contentParts), tags$br()))
}
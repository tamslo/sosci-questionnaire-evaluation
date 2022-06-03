renderSelectionQuestion <- function(questionParts, questionnaire, filterParticipants) {
  baseQuestion <- questionParts[1,]
  responses <- getResponses(baseQuestion, questionnaire, filterParticipants)
  if (all(responses$count == 0)) {
    print(paste0("[WARNING] No responses for question ", baseQuestion$id, " (", baseQuestion$text, ")"))
    return()
  }
  counts <- c()
  labels <- c()
  colors <- c()
  for (row in 1:nrow(responses)) {
    count <- responses[row, "count"]
    if (count > 0) {
      counts <- c(counts, count)
      labels <- c(labels, responses[row, "response"])
      colors <- c(colors, getColor(row, alternativeOrder = TRUE))
    }
  }
  plotParams <- list(counts = counts, labels = labels, colors = colors)
  contentParts <- list(renderPlot(baseQuestion, questionnaire, renderPie, plotParams, filterParticipants))
  showFollowUps <- getFromConfig("showFollowUps")
  if (nrow(questionParts) > 1 && showFollowUps) {
    for (row in 1:nrow(questionParts)) {
      question <- questionParts[row,]
      if (question$type == "OPEN") {
        responses <- getResponses(question, questionnaire, filterParticipants)
        if (length(responses) > 0) {
          replacements <- getFromConfig(c("replacements", "titles"))
          option <- applyReplacement(replacements, question$option)
          option <- subSectionHeading(option)
          contentParts <- append(
            contentParts,
            list(
              option,
              getOpenContent(question, questionnaire, filterParticipants)
            )
          )
        }
      }
    }
  }
  return(do.call(tagList, contentParts))
}

renderSelectionComparison <- function(questionParts, questionnaires, filterParticipants) {
  question <- questionParts[1,]
  responses <- list()
  for (questionnaire in questionnaires) {
    responses[[questionnaire]] = getResponses(question, questionnaire, filterParticipants)
  }
  answers <- list()
  colors <- c()
  labels <- c()
  for (row in 1:length(responses[[1]]$response)) {
    color <- getColor(row, alternativeOrder = TRUE)
    colors <- c(colors, color)
    label <- replaceLabel(responses[[1]][row, "response"])
    labels <- c(labels, label)
    for (questionnaire in rev(questionnaires)) {
      total <- sum(responses[[questionnaire]][, "count"])
      count <- responses[[questionnaire]][row, "count"]
      percent <- count / total * 100
      answers[[questionnaire]] <- c(answers[[questionnaire]], percent)
    }
  }
  plotParams <- list(answers = answers, colors = colors, labels = labels)
  return(renderPlot(question, "all", renderComparisonBarplot, plotParams, filterParticipants))
}
renderScaleQuestion <- function(question, questionnaire, filterParticipants) {
  questionnaires <- getQuestionnaires(question)
  globalMax <- 0
  for (currentQuestionnaire in questionnaires) {
    currentResponses <- getResponses(question, currentQuestionnaire, filterParticipants)
    currentMax <- max(currentResponses$count)
    globalMax <- max(globalMax, currentMax)
  }
  responses <- getResponses(question, questionnaire, filterParticipants)
  counts <- c()
  labels <- c()
  colors <- c()
  for (row in 1:nrow(responses)) {
    value <- responses[row, "value"]
    count <- responses[row, "count"]
    label <- responses[row, "response"]
    label <- replaceLabel(label)
    if (value > 0 || count > 0) {
      counts <- c(counts, count)
      if (value > 0 ) {
        label <- attachValueToLabel(value, label, responses)
      }
      label <- breakTextOnce(label, textLimits = c(30, 50, Inf), lineBreaks = c(15, 25, 50))
      labels <- c(labels, label)
      colors <- c(colors, getColor(row))
    }
  }
  plotParams <- list(
    counts = counts,
    labels = labels,
    ylim = globalMax,
    colors = colors
  )
  return(list(
    renderPlot(question, questionnaire, renderBarplot, plotParams, filterParticipants),
    renderStatisticsTable(question, questionnaire, filterParticipants)
  ))
}

renderScaleComparison <- function(question, questionnaires, filterParticipants) {
  responses <- list()
  for (questionnaire in questionnaires) {
    responses[[questionnaire]] = getResponses(question, questionnaire, filterParticipants)
  }
  answers <- list()
  colors <- c()
  labels <- c()
  for (row in 1:length(responses[[1]]$response)) {
    value <- responses[[1]][row, "value"]
    if (value > 0) {
      color <- getColor(row)
      colors <- c(colors, color)
      label <- attachValueToLabel(
        value,
        str_replace_all(replaceLabel(responses[[1]][row, "response"]), "\n", " "),
        responses[[1]]
      )
      labels <- c(labels, label)
      for (questionnaire in rev(questionnaires)) {
        total <- length(getCountableResponses(question, questionnaire, filterParticipants))
        count <- responses[[questionnaire]][row, "count"]
        percent <- count / total * 100
        answers[[questionnaire]] <- c(answers[[questionnaire]], percent)
      }
    }
  }
  plotParams <- list(answers = answers, colors = colors, labels = labels)
  questionnaireQuestionList <- list()
  for (questionnaire in questionnaires) {
    questionnaireQuestionList[[questionnaire]] <- question$id
  }
  return(list(
    renderPlot(question, "all", renderComparisonBarplot, plotParams, filterParticipants),
    renderStatisticsTable(question, questionnaires, filterParticipants),
    renderSignificanceTable(questionnaireQuestionList, filterParticipants)
  ))
}
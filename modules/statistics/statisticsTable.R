renderStatisticsTable <- function(question, questionnaires, filterParticipants) {
  summaryHeader <- c("", questionnaires)
  summaryContent <- list()
  countableResponsesList <- list()
  for (questionnaire in questionnaires) {
    countableResponsesList[[questionnaire]] <- getCountableResponses(question, questionnaire, filterParticipants)
  }
  responses <- getResponses(question, questionnaire, filterParticipants)
  summaryContent[[1]] <- c("Counted answers", lapply(questionnaires, function(questionnaire) {
    length(countableResponsesList[[questionnaire]])
  }))
  summaryContent[[2]] <- c("Mean answer", lapply(questionnaires, function(questionnaire) {
    getStatistic(countableResponsesList[[questionnaire]], getMean, responses)
  }))
  summaryContent[[3]] <- c("Median answer", lapply(questionnaires, function(questionnaire) {
    getStatistic(countableResponsesList[[questionnaire]], median, responses)
  }))
  summaryContent[[4]] <- c("Minimum answer", lapply(questionnaires, function(questionnaire) {
    getStatistic(countableResponsesList[[questionnaire]], min, responses)
  }))
  summaryContent[[5]] <- c("Maximum answer", lapply(questionnaires, function(questionnaire) {
    getStatistic(countableResponsesList[[questionnaire]], max, responses)
  }))
  return(htmlTable(summaryHeader, summaryContent))
}
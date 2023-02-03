getQuestionnaires <- function(question = NULL) {
  resultData <- getResultData()
  distinctQuestionnaires <- unique(resultData$QUESTNNR)
  allQuestionnaires <- distinctQuestionnaires[which(!is.na(distinctQuestionnaires))]
  if (is.null(question)) {
    questionnaires <- allQuestionnaires
  } else {
    questionnaires <- c()
    for (questionnaire in allQuestionnaires) {
      questionnaireData <- resultData[resultData$QUESTNNR == questionnaire,]
      answers <- questionnaireData[[question$id]]
      filterFunction <- function(answer) {
        !is.na(answer) && answer  != ""
      }
      givenAnswers <- Filter(filterFunction, answers)
      if (length(givenAnswers) > 0) {
        questionnaires <- c(questionnaires, questionnaire)
      }
    }
  }
  return(questionnaires)
}

notAllQuestionnairesComplete <- function() {
  resultData <- getResultData()
  participants <- resultData$SERIAL
  completeParticipants <- getCompleteParticipants(resultData)
  return(length(participants) != length(completeParticipants))
}
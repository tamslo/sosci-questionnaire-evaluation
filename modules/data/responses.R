getCountableResponses <- function(question, questionnaire, filterParticipants) {
  responses <- getResponses(question, questionnaire, filterParticipants)
  relevantResponses <- responses[responses$value > 0,]
  countedResponses <- relevantResponses[relevantResponses$count > 0,]
  responsesList <- c()
  for (row in 1:nrow(countedResponses)) {
    value <- countedResponses$value[row]
    count <- countedResponses$count[row]
    for (index in 1:count) {
      responsesList <- c(responsesList, value)
    }
  }
  return(responsesList)
}

hasTrackedParticipants <- function(resultData) {
  participantSerials <- resultData$SERIAL
  presentSerials <- which(!is.na(participantSerials))
  return(length(presentSerials) > 0)
}

getCompleteParticipants <- function(resultData, questionnaires = NULL) {
  completeParticipants <- c()
  participants <- resultData$SERIAL
  if (is.null(questionnaires)) {
    questionnaires <- unique(resultData$QUESTNNR)
  }
  for (participant in unique(participants)) {
    participantQuestionnaires <- resultData[which(resultData$SERIAL == participant), "QUESTNNR"]
    if (all(questionnaires %in% participantQuestionnaires)) {
      completeParticipants <- c(completeParticipants, participant)
    }
  }
  return(completeParticipants)
}

getResponses <- function(question, questionnaire, filterParticipants) {
  resultData <- getResultData()
  if (filterParticipants) {
    completeParticipants <- getCompleteParticipants(resultData)
    resultData <- resultData[resultData$SERIAL %in% completeParticipants,]
  }
  optionData <- getOptionData()
  type <- question$type
  id <- question$id
  if (length(questionnaire) > 1) {
    stop("too many questionnaires")
  }
  questionnaireResults <- resultData[resultData$QUESTNNR == questionnaire,]
  questionResults <- questionnaireResults[id]
  if (type == "OPEN") {
    responses <- questionResults[questionResults != ""]
  } else if (type == "SELECTION" || type == "SCALE" || type == "CHECKBOX") {
    possibleAnswers <- optionData[optionData$VAR == id,]
    response <- c()
    value <- c()
    count <- c()
    for (row in 1:nrow(possibleAnswers)) {
      response <- c(response, possibleAnswers$MEANING[row])
      value <- c(value, possibleAnswers$RESPONSE[row])
      currentCount <- length(which(questionResults == possibleAnswers$RESPONSE[row]))
      count <- c(count, currentCount)
    }
    responses <- data.frame(response, value, count)
  } else {
    print(paste("Warning: Unknown type", type))
    responses <- NA
  }

  return(responses)
}
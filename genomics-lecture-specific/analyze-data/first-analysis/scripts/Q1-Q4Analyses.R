sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

resultData <- getResultData()
completeParticipants <- getCompleteParticipants(resultData, c("Q1", "Q2", "Q3", "Q4_HPI"))
completeParticipantsResults <- resultData[resultData$SERIAL %in% completeParticipants,]
preCourseAnswers <- completeParticipantsResults[completeParticipantsResults$QUESTNNR == "Q1",]
intermediateAnswers <- completeParticipantsResults[completeParticipantsResults$QUESTNNR == "Q2",]
endCourseAnswers <- completeParticipantsResults[completeParticipantsResults$QUESTNNR == "Q3",]
retrospectiveAnswers <- completeParticipantsResults[completeParticipantsResults$QUESTNNR == "Q4_HPI",]

checkAnalysesAnwers <- function() {
  analysesAnswers <- retrospectiveAnswers$Q411
}

checkCounselingAnswer <- function() {
  print(preCourseAnswers[,c("SERIAL","RS02_03")])
  print(intermediateAnswers[,c("SERIAL","RS02_03")])
  print(endCourseAnswers[,c("SERIAL","RS02_03")])
  print(retrospectiveAnswers[,c("SERIAL","Q414")])
  optimisticParticipantQ1 <- preCourseAnswers[preCourseAnswers$RS02_03 == 4,]$SERIAL
  optimisticParticipantQ4 <- retrospectiveAnswers[retrospectiveAnswers$Q414 == 1,]$SERIAL
}
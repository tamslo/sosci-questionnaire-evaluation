sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

results <- getResultData()
questionnaires <- unique(results$QUESTNNR)
allParticipants <- unique(results$SERIAL)
participantsMap <- list()
for (participant in allParticipants) {
  participantsMap[[participant]] <- c()
  for (questionnaire in questionnaires) {
    questionnaireParticipants <- results$SERIAL[which(results$QUESTNNR == questionnaire)]
    participantsMap[[participant]] <- c(participantsMap[[participant]], participant %in% questionnaireParticipants)
  }
}
participantsFrame <- data.frame(t(sapply(participantsMap,c)))
colnames(participantsFrame) <- questionnaires

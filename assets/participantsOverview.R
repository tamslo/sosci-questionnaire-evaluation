library(rjson)
sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

resultData <- getResultData()
questionnaires <- unique(resultData$QUESTNNR)
participants <- unique(resultData$SERIAL)

# Matrix participants x questionnaires

participations <- c()
for (participant in participants) {
  participantQuestionnaires <- resultData[resultData$SERIAL == participant,]$QUESTNNR
  participation <- c()
  for (questionnaire in questionnaires) {
    participation <- c(participation, questionnaire %in% participantQuestionnaires)
  }
  participations <- c(participations, participation)
}

participationData <- as.data.frame(matrix(
  participations,
  ncol = 4,
  byrow = TRUE,
  dimnames = list(participants, questionnaires)
))

View(participationData)

# Number of participants per questionnaire combination

combinations <- list()
combinationIndex <- 0
for (combinationLength in 1:length(questionnaires)) {
  lenghtCombinations <- combn(questionnaires, combinationLength)
  for (combinationColumn in 1:ncol(lenghtCombinations)) {
    combinationIndex <- combinationIndex + 1
    lengthCombination <- lenghtCombinations[,combinationColumn]
    combinations[[combinationIndex]] <- lengthCombination
  }
}
combinationsParticipants <- c()
combinationNames <- c()
for (combination in combinations) {
  combinationNames <- c(combinationNames, paste(combination, collapse = "–"))
  combinationData <- as.data.frame(participationData[,combination])
  colnames(combinationData) <- combination
  combinationParticipants <- 0
  for (rowIndex in 1:nrow(combinationData)) {
    participationNumber <- sum(as.logical(combinationData[rowIndex, combination]))
    if (participationNumber == length(combination)) {
      combinationParticipants <- combinationParticipants + 1
    }
  }
  combinationsParticipants <- c(combinationsParticipants, combinationParticipants)
}
combinationInformation <- data.frame(combinationNames, combinationsParticipants)
colnames(combinationInformation) <- c("combination", "participants")
View(combinationInformation)

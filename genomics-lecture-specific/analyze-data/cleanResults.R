# Cleaning methods for results data
# Regarding default question IDs normalized data is assumed

withConsent <- function(results, consentQuestion = "QX02") {
  noAnswer <- 2
  questionnairesWithExternalConsent <- c("Q1", "Q2", "Q3")
  return(results[which(
    results[consentQuestion] != noAnswer |
      (results$QUESTNNR %in% questionnairesWithExternalConsent & is.na(results[consentQuestion]))),])
}

removeTumRetrospectiveFutureAnalysesWithoutPassword <- function(resultData, passwordQuestionId = "Q402", furtherAnalysesQuestionId = "Q405") {
  # The question "Do you plan to conduct further analyses with your own genomic data?"
  # should have only been asked TUM students who collected their passwords (makes more sense
  # because of information contained in data)
  noAnswer <- 2
  for (rowIndex in 1:nrow(resultData)) {
    questionnaire <- resultData$QUESTNNR[rowIndex]
    receivedPassword <- resultData[rowIndex, passwordQuestionId]
    if (questionnaire == "TUM_Retrospective" & (is.na(receivedPassword) | receivedPassword == noAnswer)) {
      resultData[rowIndex, furtherAnalysesQuestionId] <- NA
    }
  }
  return(resultData)
}

cleanGenotypingDependencies <- function(data) {
  # Q402 (password collection) and Q403 (plan to collect password) should only have been asked
  # if QX08 (participation in genotyping) was answered with yes, but apparently this did
  # not work in all questionnaires; cleaning this here
  # (No need to clean for analyses questions, here the questionnaire worked as expected)
  for (rowIndex in 1:nrow(data)) {
    genotypingParticipation <- data[rowIndex,"QX08"]
    if (!is.na(genotypingParticipation)) {
      yesAnswer <- "1"
      noAnswer <- "2"
      if (genotypingParticipation == noAnswer) {
        didCollectPasswordQuestion <- "Q402"
        planCollectPasswordQuestion <- "Q403"
        cleanAnswer <- function(data, rowIndex, questionId) {
          answer <- data[rowIndex, questionId]
          if (!is.na(answer)) {
            text <- paste0("[LOG_STAGE] Answer of ", questionId, " was ANSWER; setting to NA.")
            if (answer == noAnswer) {
              text <- stringr::str_replace_all(text, "LOG_STAGE", "INFO")
              text <- stringr::str_replace_all(text, "ANSWER", "no (as desired)")
            } else if (answer == yesAnswer) {
              text <- stringr::str_replace_all(text, "LOG_STAGE", "WARNING")
              text <- stringr::str_replace_all(text, "ANSWER", "yes (conflicts with PGT answer)")
            } else {
              text <- stringr::str_replace_all(text, "LOG_STAGE", "WARNING")
              text <- stringr::str_replace_all(text, "ANSWER", paste0(answer, " (unknown, expected 1 or 2)"))
            }
            print(text)
            data[rowIndex, questionId] <- NA
          }
          return(data)
        }
        data <- cleanAnswer(data, rowIndex, didCollectPasswordQuestion)
        data <- cleanAnswer(data, rowIndex, planCollectPasswordQuestion)
      }
    }
  }
  return(data)
}
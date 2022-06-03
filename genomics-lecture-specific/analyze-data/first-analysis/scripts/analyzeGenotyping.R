sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
source("genomics-lecture-specific/analyze-data/first-analysis/scripts/utils.R")

resultData <- getResultData()

hpiGenotypingData <- resultData[which(resultData$QUESTNNR == "Q2" |
                                        resultData$QUESTNNR == "Q3" |
                                        resultData$QUESTNNR == "Q4_HPI"),
                                c("SERIAL", "DU14", "Q405")]
results <- c()
for (participant in unique(hpiGenotypingData$SERIAL)) {
  participantData <- hpiGenotypingData[which(hpiGenotypingData$SERIAL == participant),]
  participantResponses <- c()
  for (row in 1:nrow(participantData)) {
    participantResponse <- as.numeric(participantData[row, c("DU14", "Q405")])
    participantResponse <- participantResponse[which(!is.na(participantResponse))]
    participantResponses <- c(participantResponses, participantResponse)
  }
  results <- c(results, min(participantResponses))
}
optionData <- getOptionData()
questionOptions <- optionData[which(optionData$VAR == "DU14"),]
plotPath <- file.path(resultDirectory, "hpi_genotyping.png")
plotPie(results, questionOptions, plotPath)


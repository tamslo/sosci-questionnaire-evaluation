source("assets/stratification/utils.R")

analyzePastAnalyses <- function(tumResults, hpiResults, optionData, resultName, tumQuestion, hpiQuestion, resultQuestionId) {
  tumAnalysesResponses <- tumResults[, tumQuestion]
  tumAnalysesResponses <- tumAnalysesResponses[which(
    !is.na(tumAnalysesResponses) & tumAnalysesResponses > 0)]
  tumAnswers <- c()
  for (response in tumAnalysesResponses) {
    responseOption <- optionData[which(optionData$VAR == tumQuestion &
                                         optionData$RESPONSE == response),]
    tumAnswers <- c(tumAnswers, responseOption$MEANING)
  }
  hpiAnalysesResponses <- hpiResults[, which(
    startsWith(colnames(hpiResults), hpiQuestion) &
      !endsWith(colnames(hpiResults), "_01") # "I just want to receive my data"
  )]
  hpiAnswers <- c()
  for (rowIndex in 1:nrow(hpiAnalysesResponses)) {
    row <- as.numeric(hpiAnalysesResponses[rowIndex,])
    # Checked: 2
    if (any(!is.na(row) & row == 2)) {
      hpiAnswers <- c(hpiAnswers, "Yes")
    } else if (all(!is.na(row) & row == 1)) {
      hpiAnswers <- c(hpiAnswers, "No")
    }
  }
  
  drawPie <- function(uni, questionId, answers) {
    counts <- c()
    labels <- unique(answers)
    colors <- c()
    for (index in 1:length(labels)) {
      answer <- labels[index]
      counts <- c(counts, length(which(answers == answer)))
      colors <- c(colors, getColor(index, alternativeOrder = TRUE))
    }
    plotPath <- getSinglePlotPath(questionId, resultName, uni)
    createPie(plotPath, labels, counts, colors, uni)
    return(plotPath)
  }
  tumPath <- drawPie("TUM", tumQuestion, tumAnswers)
  hpiPath <- drawPie("HPI", hpiQuestion, hpiAnswers)
  outputPath <- getComparisonResultPath(resultQuestionId, resultName)
  questionText <- "Have you conducted analyses in the past?"
  combinePlots(outputPath, c(tumPath, hpiPath), questionText)

  valueMatrix <- data.frame(
    "population" = c(rep("HPI", length(hpiAnswers)), rep("TUM", length(tumAnswers))),
    "value" = c(hpiAnswers, tumAnswers)
  )
  firstGroupValues <- getValueString(valueMatrix, "HPI")
  secondGroupValues <- getValueString(valueMatrix, "TUM")
  statistics <- adjustPValues(addToStatistics(
    c(resultName, "HPI <> TUM", resultQuestionId, questionText, getStatistics(valueMatrix), firstGroupValues, secondGroupValues)))
  write.csv(statistics,
            getComparisonResultPath(resultQuestionId, resultName, type = "csv"),
            row.names = FALSE)
}


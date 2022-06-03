library(stringr)
sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
source("assets/stratification/utils.R", local = TRUE)

motivationTexts <- c(
  "General interest in genomics/genomic analyses",
  "To receive/analyze my own genomic data",
  "To apply gained knowledge in my professional career",
  "To receive the credit points",
  "To learn about tools for variant interpretation and analysis",
  "Interest in ancestry analysis",
  "Interest in pharmacogenomics",
  "Interest in research topics like genome-wide association studies",
  "Interest in commercial genomic ('DTC') testing",
  "To better understand the situation of patients",
  "Interest in ethical issues in the context of genomic analyses",
  "Interest in legal foundations of genomic analyses"
)

getMotivationData <- function(dataSpecification) {
  getMotivationDataForGroup <- function(questionnaireId, motivationQuestionId, stratify = NULL) {
    resultData <- getResultData()
    resultData <- resultData[which(resultData$Q402 != 2 | resultData$T102 != 2),]
    questionData <- getQuestionData()
    id <- c()
    motivation <- c()
    responses <- c()
    total <- c()
    
    questionnaireData <- resultData[which(resultData$QUESTNNR == questionnaireId),]
    if (!is.null(stratify)) {
      stratificationQuestion <- stratify[["question"]]
      stratificationAnswers <- stratify[["answers"]]
      questionnaireData <- questionnaireData[which(
        questionnaireData[[stratificationQuestion]] %in% stratificationAnswers
      ),]
    }
    totalResponses <- nrow(questionnaireData)
    motivationResponses <- questionnaireData[, which(
      startsWith(names(questionnaireData), motivationQuestionId)
    )]
    motivationOptions <- questionData[which(
      startsWith(questionData$VAR, paste(motivationQuestionId, "_", sep = ""))
    ),]
    # 1 - Not checked
    # 2 - Checked
    for (row in 1:nrow(motivationOptions)) {
      motivationOption <- motivationOptions[row,]
      id <- c(id, motivationOption$VAR)
      motivation <- c(motivation, str_replace(
        motivationOption$LABEL, "Motivation: ", ""
      ))
      numberOfResponses <- length(which(
        motivationResponses[motivationOption$VAR] == 2
      ))
      responses <- c(responses, numberOfResponses / totalResponses)
      total <- c(total, totalResponses)
    }
    motivationData <- data.frame(id, motivation, responses, total)
  }
  
  texts <- c()
  labels <- c()
  colors <- c()
  populations <- c()
  values <- c()
  meanValue <- c()
  total <- c()
  for (index in 1:length(motivationTexts)) {
    getDataValue <- function(data, ids, index, fieldName) {
      id <- ids[index]
      value <- 0
      if (!is.na(id)) {
        value <- data[which(data$id == id), fieldName]
      }
      return(value)
    }
    currentLabels <- rep(NA, length(dataSpecification))
    displayLabel <- motivationTexts[index]
    currentLabels[min(2, length(currentLabels))] <- displayLabel
    labels <- c(labels, currentLabels)
    currentValues <- c()
    for (populationName in names(dataSpecification)) {
      questionnaireId <- dataSpecification[[populationName]][["questionnaire"]]
      questionId <- dataSpecification[[populationName]][["question"]]
      stratification <- dataSpecification[[populationName]][["stratification"]]
      populationIds <- dataSpecification[[populationName]][["ids"]]
      populationColor <- dataSpecification[[populationName]][["color"]]
      populationData <- getMotivationDataForGroup(questionnaireId, questionId, stratification)
      colors <- c(colors, populationColor)
      populations <- c(populations, populationName)
      texts <- c(texts, displayLabel)
      currentValues <- c(currentValues, getDataValue(populationData, populationIds, index, "responses"))
      total <- c(total, getDataValue(populationData, populationIds, index, "total"))
    }
    values <- c(values, currentValues)
    meanValue <- c(
      meanValue,
      rep(mean(currentValues), length(dataSpecification))
    )
  }
  results <- data.frame(texts, labels, colors, populations, values, meanValue, total)
  results <- results[order(results$meanValue),]
  return(results)
}

getMotivationTableData <- function(dataSpecification) {
  data <- getMotivationData(dataSpecification)
  tableData <- data.frame("Question" = unique(data$texts))
  for (population in data$populations) {
    populationValues <- c()
    for (ratio in data[which(data$populations == population),]$values) {
      roundedPercentage <- round(ratio * 100, digits = 1)
      populationValues <- c(populationValues, paste(roundedPercentage, "%"))
    }
    tableData[population] <- populationValues
  }
  tableData$Mean <- paste(round(unique(data$meanValue) * 100, digits = 1), "%")
  return(tableData[order(tableData$Mean, decreasing = TRUE),])
}

computeMotivationResults <- function(dataSpecification, outputQuestionId, resultName) {
  plotData <- getMotivationData(dataSpecification)
  
  # Statistics
  data <- plotData[, c("texts", "populations", "values", "total")]
  colnames(data) <- c("question", "population", "selected.ratio", "total")

  statistics <- NULL
  descriptivePrefix <- "Motivated by:"
  comparisons <- combn(names(dataSpecification), 2)
  for (motivationText in motivationTexts) {
    comparisonStatistics <- NULL
    for (comparisonIndex in 1:ncol(comparisons)) {
      populationNames <- comparisons[, comparisonIndex]
      firstPopulationName <- populationNames[1]
      secondPopulationName <- populationNames[2]
      getPopulationAnswers <- function(populationName, data, motivationText) {
        populationData <- data[which(data$population == populationName & data$question == motivationText),]
        selectedRatio <- populationData$selected.ratio
        total <- populationData$total
        yesAnswers <- total * selectedRatio
        noAnswers <- total - yesAnswers
        return(c(rep("Yes", yesAnswers), rep("No", noAnswers)))
      }
      firstPopulationData <- getPopulationAnswers(firstPopulationName, data, motivationText)
      secondPopulationData <- getPopulationAnswers(secondPopulationName, data, motivationText)
      population <- c(
        rep(firstPopulationName, length(firstPopulationData)),
        rep(secondPopulationName, length(secondPopulationData))
      )
      value <- c(firstPopulationData, secondPopulationData)
      valueMatrix <- data.frame(population, value)
      comparisonStatistics <- addToStatistics(c(
        resultName, paste(populationNames, collapse = " <> "), outputQuestionId,
        paste(descriptivePrefix, motivationText), getStatistics(valueMatrix),
        getValueString(valueMatrix, firstPopulationName),
        getValueString(valueMatrix, secondPopulationName)
      ), comparisonStatistics)
    }
    comparisonStatistics <- adjustPValues(comparisonStatistics)
    statistics <- rbind(statistics, comparisonStatistics)
  }
  write.csv(statistics, getComparisonResultPath(outputQuestionId, resultName, type = "csv"),
            row.names = FALSE)
  
  # Add p-values to labels
  labelsWithStats <- c()
  for (rowIndex in 1:nrow(plotData)) {
    row <- plotData[rowIndex,]
    if (!is.na(row$labels)) {
      label <- row$labels
      statsLabel <- paste(descriptivePrefix, label)
      stats <- statistics[which(statistics$question.text == statsLabel),]
      if (stats$p.value != "Inf") {
        pValue <- round(as.numeric(stats$p.value), digits = 2)
        effectSize <- round(as.numeric(stats$effect.size), digits = 2)
        postFix <- ""
        if (pValue < 0.05) {
          postFix <- "*"
        }
        newLabel <- paste(label, " (p = ", pValue, ", V = ", effectSize, ")", postFix, sep = "")
        plotData[rowIndex, "labels"] <- newLabel
      }
    }
  }
  #View(plotData)
  #View(statistics)
  
  plotMotivations <- function(results) {
    betweenPopulationsSpace <- 1
    betweenQuestionsSpace <- 3
    barSize <- 1
    populationsNumber <- length(unique(results$populations))
    questionsNumber <- length(unique(results$texts))

    # Legend with (N = participants)
    legendTexts <- c()
    for (population in unique(results$populations)) {
      populationNumber = max(results$total[which(results$populations == population)])
      legendTexts <- c(legendTexts, paste0(population, " (N = ", populationNumber, ")"))
    }
    # Get absolute values to annotate bars
    absoluteValues <- c()
    absoluteValuesYPositions <- c()
    for (index in 1:length(results$values)) {
      absoluteValueYPosition <- index + 0.5 + 2 * barSize # spacing from bottom
      absoluteValueYPosition <- absoluteValueYPosition + betweenPopulationsSpace * floor((index) / populationsNumber)
      absoluteValueYPosition <- absoluteValueYPosition + betweenQuestionsSpace * floor((index - 1) / populationsNumber)
      absoluteValuesYPositions <- c(absoluteValuesYPositions, absoluteValueYPosition)
      value <- results$values[index]
      total <- results$total[index]
      if (results$total[index] > 0) {
        absoluteValue <- as.character(value * total)
      } else {
        absoluteValue <- ""
      }
      absoluteValues <- c(absoluteValues, absoluteValue)
    }
    par(mai=c(1,6.6,1,1))
    space <- rep(c(betweenQuestionsSpace, rep(betweenPopulationsSpace, populationsNumber - 1)), questionsNumber)
    barplot(results$values * 100, col = results$colors, horiz = TRUE,
            space = space, border = NA)
    abline(v = seq(0, 100, by = 5), col = "grey", lty = "solid")
    abline(v = seq(0, 100, by = 1), col = "grey", lty = "dotted")
    barplot(add = TRUE, results$values  * 100, col = results$colors, horiz = TRUE,
            space = space, border = NA, width = rep(barSize, length(results$values)),
            names.arg = results$labels, las = 1, ylab = "%")
    legend("bottomright", legend = rev(legendTexts),
           col = rev(unique(results$colors)), pch = 15, box.lwd = 0, inset = c(0.01, 0.01))
    text(c(1.6, 22.1, 42.1, 62.1, 82.1, 102.6), y = -4.6, labels = "%", xpd = TRUE)
    text(x = (results$values * 100 + 2.5), y = absoluteValuesYPositions, labels = absoluteValues, xpd = TRUE)
  }
  plotPath <- getComparisonResultPath(outputQuestionId, resultName)
  createPng(plotMotivations, plotData, plotPath, width = 1200, height = 460)
  createPdf(plotMotivations, plotData, str_replace(plotPath, ".png", ".pdf"), width = 16, height = 6.7)
}

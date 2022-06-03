#sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
source("modules/dataPath.R", local = TRUE)
source("modules/data/load.R", local = TRUE)
source("modules/data/questions.R", local = TRUE)
source("modules/view/plotting/color.R", local = TRUE)
source("modules/view/plotting/rendering.R", local = TRUE)
source("modules/view/plotting/pie.R", local = TRUE)

library(stringr)
library(fs)

library(png)
library(grid)
library(gridExtra)

library(lsr)
library(statmod)
library(DescTools)

SINGLE_PLOT_WIDTH = 400
SINGLE_PLOT_HEIGHT = 500
PLOT_TEXT_BREAK = 15

CHI_SQ <- "chiSq"
FISHER <- "fisher"
SIGNIFICANCE_TEST <- FISHER
EFFECT_MEASURE <- "cramersV"

getResultDirectory <- function(subDirectory = NULL) {
  parentDirectory <- "results"
  if (!dir.exists(parentDirectory)) {
    dir.create(parentDirectory)
  }
  resultDirectoryName <- str_replace_all(str_remove(getDataPath(), "data/"), "/", "_")
  resultDirectory <- file.path(parentDirectory, resultDirectoryName)
  if (!dir.exists(resultDirectory)) {
    dir.create(resultDirectory)
  }
  if (!is.null(subDirectory)) {
    resultDirectory <- file.path(resultDirectory, subDirectory)
    if (!dir.exists(resultDirectory)) {
      dir.create(resultDirectory)
    }
  }
  return(resultDirectory)
}

getSingleComparisonResultDirectory <- function(parentDirectory = NULL) {
  return(file.path(getResultDirectory(parentDirectory), "single"))
}

getSinglePlotPath <- function(questionId, resultName, populationName, parentDirectory = NULL) {
  singleResultDirectory <- getSingleComparisonResultDirectory(parentDirectory)
  if (!dir.exists(singleResultDirectory)) {
    dir.create(singleResultDirectory)
  }
  plotName <- paste(questionId, "_", resultName, "_", populationName)
  plotName <- str_replace_all(plotName, " ", "_")
  plotName <- path_sanitize(plotName)
  return(file.path(singleResultDirectory, paste(plotName, ".png", sep = "")))
}

getComparisonResultPath <- function(questionId, resultName, type = "png", parentDirectory = NULL) {
  fileName <- paste(questionId, "_", resultName, ".", type, sep = "")
  return(file.path(getResultDirectory(parentDirectory), fileName))
}

combineStatistics <- function(resultNames, questionIds, combinedOutputDirectory = NULL) {
  combinedStatistics <- NULL
  for (questionId in questionIds) {
    for (resultName in resultNames) {
      statisticsPath <- getComparisonResultPath(questionId, resultName, type = "csv", parentDirectory = combinedOutputDirectory)
      if (file.exists(statisticsPath)) {
        statistics <- read.csv(statisticsPath)
        if (is.null(combinedStatistics)) {
          combinedStatistics <- statistics
        } else {
          combinedStatistics <- rbind(combinedStatistics, statistics)
        }
      }
    }
  }
  return(combinedStatistics)
}

getStatistics <- function(valueMatrix) {
  contingencyTableColumns <- length(unique(valueMatrix$value))
  contingencyTableRows <- length(unique(valueMatrix$population))
  pValue <- Inf
  degreesOfFreedom <- (contingencyTableColumns - 1) * (contingencyTableRows - 1)
  testWarning <- NA
  effectSize <- -Inf
  strictEffectSizeInterpretation <- "undefined"
  nearestEffectSizeInterpretation <- "undefined"
  power <- -Inf
  if (contingencyTableColumns > 1 & contingencyTableRows > 1) {
    if (SIGNIFICANCE_TEST == FISHER) {
      runSignificanceTest <- function(valueMatrix) {
        return(fisher.test(valueMatrix$population, valueMatrix$value))
      }
    } else { # SIGNIFICANCE_TEST == CHI_SQ
      runSignificanceTest <- function(valueMatrix) {
        return(chisq.test(valueMatrix$population, valueMatrix$value))
      }
    }
    significanceTest <- runSignificanceTest(valueMatrix)
    pValue <- significanceTest[["p.value"]]
    # Run test again only to catch warning
    testWarning <- tryCatch({
      runSignificanceTest(valueMatrix)
    }, warning = function(warningObject) {
      return(conditionMessage(warningObject))
    }, finally = function() {})
    if (typeof(testWarning) != "character") {
      testWarning <- NA
    }
    effectSize <- cramersV(table(valueMatrix))
    effectSizeInterpretationData <- data.frame(
      "degreesOfFreedom" = c(1, 2, 3, 4, 5),
      "small" = c(0.10, 0.07, 0.06, 0.05, 0.04),
      "medium" = c(0.30, 0.21, 0.17, 0.15, 0.13),
      "large" = c(0.50, 0.35, 0.29, 0.25, 0.22))
    categories <- names(effectSizeInterpretationData)[2:4]
    thresholds <- as.numeric(effectSizeInterpretationData[which(
      effectSizeInterpretationData$degreesOfFreedom == degreesOfFreedom), categories])
    if (effectSize < thresholds[1]) {
      strictEffectSizeInterpretation <- categories[1]
    } else if (effectSize < thresholds[2]) {
      strictEffectSizeInterpretation <- paste0(categories[1], " to ", categories[2])
    } else if (effectSize == thresholds[2]) {
      strictEffectSizeInterpretation <- categories[2]
    } else if (effectSize < thresholds[3]) {
      strictEffectSizeInterpretation <- paste0(categories[2], " to ", categories[3])
    } else {
      strictEffectSizeInterpretation <- categories[3]
    }
    nearestEffectSizeInterpretation <- categories[which(abs(thresholds-effectSize) == min(abs(thresholds-effectSize)))]
    # Actually would need Phi instead of Cramer's V; maybe also only for 2x2 contingency table
    power <- power.chisq.test(n = length(valueMatrix$value), w = effectSize, df = degreesOfFreedom)[["power"]]
  }
  return(c(SIGNIFICANCE_TEST, pValue, (pValue < 0.05), degreesOfFreedom, testWarning, EFFECT_MEASURE, effectSize,
           strictEffectSizeInterpretation, nearestEffectSizeInterpretation, power))
}

statisticColumnNames <- c("significance.test",
                          "p.value", "uncorrected.is.significant", "degees.of.freedom", "test.warning",
                          "effect.measure", "effect.size", "strict.effect.size.interpretation", "nearest.effect.size.interpretation",
                          "approx.power.chi.squared")
extendedStatisticColumnNames <- c("stratification", "comparison", "question.id", "question.text", statisticColumnNames, "first.group.values", "second.group.values", "adjusted.p.over.comparison.tests",
                                  "adjusted.is.significant")

addToStatistics <- function(newValues, statistics = NULL) {
  columnNames <- extendedStatisticColumnNames
  adjustedPValue <- NA
  isSignificant <- NA
  values <- c(newValues, adjustedPValue, isSignificant)
  statistics <- rbind(statistics, values)
  colnames(statistics) <- columnNames
  statistics <- as.data.frame(statistics)
  return(statistics)
}

getValueString <- function(valueMatrix, populationName) {
  valueString <- paste(populationName, ":", sep = "")
  populationValues <- valueMatrix[which(valueMatrix$population == populationName), "value"]
  if (length(populationValues) > 0) {
    populationValues <- sort(populationValues)
    for (value in unique(populationValues)) {
      valueString <- paste(valueString,
                           paste(value, " (",
                                 length(which(populationValues == value)),
                                 ")", sep = ""))
    }
  } else {
    valueString <- paste(valueString, "Question not asked")
  }
  return(valueString)
}

adjustPValues <- function(statistics, adjustedField = "adjusted.p.over.comparison.tests", significantField = "adjusted.is.significant") {
  statistics[adjustedField] <- p.adjust(statistics$p.value, method = "fdr")
  statistics[significantField] <- statistics[adjustedField] < 0.05
  return(statistics)
}

runStatisticalTests <- function(questionId, populations, resultName, questionText) {
  optionData <- getOptionData()
  questionOptions <- optionData[which(optionData$VAR == questionId),]
  comparisons <- combn(names(populations), 2)
  statistics <- NULL
  for (comparisonIndex in 1:ncol(comparisons)) {
    populationNames <- comparisons[, comparisonIndex]
    getValueMatrix <- function(questionId, populations, populationNames) {
      firstPopulationValues <- populations[[populationNames[1]]][[questionId]]
      secondPopulationValues <- populations[[populationNames[2]]][[questionId]]
      combinedValues <- c(firstPopulationValues, secondPopulationValues)
      population <- c()
      value <- c()
      for (index in 1:length(combinedValues)) {
        populationName <- populationNames[1]
        if (index > length(firstPopulationValues)) {
          populationName <- populationNames[2]
        }
        valueResponse <- combinedValues[index]
        if (!is.na(valueResponse) & nrow(questionOptions) > 0) {
          population <- c(population, populationName)
          valueMeaning <- questionOptions[which(questionOptions$RESPONSE == valueResponse), "MEANING"]
          value <- c(value, valueMeaning)
        }
      }
      return(data.frame(population, value))
    }

    valueMatrix <- getValueMatrix(questionId, populations, populationNames)
    comparisonName <- paste(populationNames, collapse = "<>")
    firstGroupValues <- getValueString(valueMatrix, populationNames[1])
    secondGroupValues <- getValueString(valueMatrix, populationNames[2])
    statistics <- addToStatistics(
      c(resultName, comparisonName, questionId, questionText, getStatistics(valueMatrix), firstGroupValues, secondGroupValues),
      statistics)
  }
  return(adjustPValues(statistics))
}

createPie <- function(plotPath, labels, counts, colors, title, hasAnswers = TRUE) {
  plotParams <- list("counts" = counts, "labels" = labels, "colors" = colors,
                     "title" = title, "breakPosition" = PLOT_TEXT_BREAK)
  createPng(renderPie, plotParams, plotPath, width = SINGLE_PLOT_WIDTH, height = SINGLE_PLOT_HEIGHT)
}

plotPie <- function(questionResults, questionOptions, plotPath, title = "") {
  labels <- c()
  counts <- c()
  colors <- c()
  hasAnswers <- FALSE
  for (row in 1:nrow(questionOptions)) {
    questionOption <- questionOptions[row,]
    optionLabel <- questionOption$MEANING
    optionCount <- length(which(questionResults == questionOption$RESPONSE))
    if (optionCount > 0) {
      hasAnswers <- TRUE
      labels <- c(labels, optionLabel)
      counts <- c(counts, optionCount)
      colors <- c(colors, getColor(row, alternativeOrder = TRUE))
    }
  }
  if (!hasAnswers) {
    labels <- c("Question not asked")
    counts <- c(length(questionResults))
    colors <- c(colors, getColor(6, alternativeOrder = TRUE))
  }
  createPie(plotPath, labels, counts, colors, title, hasAnswers)
}

buildPopulations <- function(basePopulation, stratificationQuestion, groups = NULL) {
  if (is.null(groups)) {
    groups <- list()
    optionData <- getOptionData()
    stratificationOptions <- optionData[which(optionData$VAR == stratificationQuestion),]
    for (row in 1:nrow(stratificationOptions)) {
      option <- stratificationOptions[row,]
      optionName <- option$MEANING
      optionValue <- option$RESPONSE
      groups[[optionName]] <- optionValue
    }
  }
  populations <- list()
  for (populationName in names(groups)) {
    answerValues <- c(groups[[populationName]])
    answers <- basePopulation[which(basePopulation[[stratificationQuestion]] %in% answerValues),]
    if (nrow(answers) > 0) {
      populations[[populationName]] <- answers
    }
  }
  return(populations)
}

combinePlots <- function(outputPath, plotPaths, title = "", text = "", maxCols = 3,
                         singlePlotWidth = SINGLE_PLOT_WIDTH, singlePlotHeight = SINGLE_PLOT_HEIGHT) {
  numberOfPlots <- length(plotPaths)
  cols <- min(numberOfPlots, maxCols)
  rows <- ceiling(numberOfPlots / cols)
  grobs <- list()
  layoutMatrixVector <- c()
  layoutMatrixCols <- cols

  if (numberOfPlots %% cols == 0) {
    for (index in 1:numberOfPlots) {
      grobs[[index]] <- rasterGrob(readPNG(plotPaths[index]))
      layoutMatrixVector <- c(layoutMatrixVector, index)
    }
  } else {
    layoutMatrixCols <- cols * 2
    lastRowPlots <- numberOfPlots %% cols
    lastRegularPlot <- numberOfPlots - lastRowPlots
    missingGrobParts <- cols - lastRowPlots

    # First write all plots into grobs until last regular plot
    for (index in 1:lastRegularPlot) {
      grobs[[index]] <- rasterGrob(readPNG(plotPaths[index]))
      layoutMatrixVector <- c(layoutMatrixVector, index, index)
    }
    
    # Fill last row with leading null grobs
    firstLeadingGrobIndex <- lastRegularPlot + 1
    lastLeadingGrobIndex <- lastRegularPlot + missingGrobParts
    for (index in firstLeadingGrobIndex:lastLeadingGrobIndex) {
      grobs[[index]] <- grid::nullGrob()
      layoutMatrixVector <- c(layoutMatrixVector, index)
    }
    
    # Enter last row grobs
    firstPlotIndex <- lastLeadingGrobIndex + 1
    lastPlotIndex <- lastLeadingGrobIndex + lastRowPlots
    for (index in firstPlotIndex:lastPlotIndex) {
      grobs[[index]] <- rasterGrob(readPNG(plotPaths[index - missingGrobParts]))
      layoutMatrixVector <- c(layoutMatrixVector, index, index)
    }
    
    # Fill last row with trailing null grobs
    firstTrailingGrobIndex <- lastPlotIndex + 1
    lastTrailingGrobIndex <- lastPlotIndex + missingGrobParts
    for (index in firstTrailingGrobIndex:lastTrailingGrobIndex) {
      grobs[[index]] <- grid::nullGrob()
      layoutMatrixVector <- c(layoutMatrixVector, index)
    }
  }
  layoutMatrix <- matrix(layoutMatrixVector, byrow = TRUE, ncol = layoutMatrixCols)

  raster_parameters <- list()
  raster_parameters[["grobs"]] <- grobs
  raster_parameters[["layout_matrix"]] <- layoutMatrix
  raster_parameters[["top"]] <- textGrob(breakText(title, 100), gp=gpar(fontsize=4))
  raster_parameters[["bottom"]] <- textGrob(text, gp = gpar(fontsize=4))

  imageParams <- list(
    "filename" = outputPath,
    "width" = singlePlotWidth * maxCols,
    "height" = singlePlotHeight * rows,
    "unit" = "px",
    "res" = 300,
    "pointsize" = 12
  )
  do.call(png, imageParams)
  do.call(grid.arrange, raster_parameters)
  dev.off()
}

comparePopulations <- function(questionId, populationResults, resultName, combinedOutputDirectory) {
  optionData <- getOptionData()
  plotPaths <- c()
  for (populationName in names(populationResults)) {
    populationResult <- populationResults[[populationName]]
    questionResults <- populationResult[[questionId]]
    questionOptions <- optionData[which(optionData$VAR == questionId),]
    plotPath <- getSinglePlotPath(questionId, resultName, populationName, combinedOutputDirectory)
    title <- str_replace_all(populationName, "_", " ")
    plotPie(questionResults, questionOptions, plotPath, title)
    plotPaths <- c(plotPaths, plotPath)
  }
  questionData <- getQuestions()
  question <- questionData[which(questionData$id == questionId),]
  questionTitle <- getQuestionTitle(question)
  combinePlots(getComparisonResultPath(questionId, resultName, parentDirectory =  combinedOutputDirectory), plotPaths, questionTitle)
  statistics <- runStatisticalTests(questionId, populationResults, resultName, questionTitle)
  write.csv(statistics,
            getComparisonResultPath(questionId, resultName, type = "csv", parentDirectory = combinedOutputDirectory),
            row.names = FALSE)
}

runComparisons <- function(comparisonSpecification, combinedOutputFileName = NULL, combinedOutputDirectory = NULL) {
  allQuestions <- c()
  for (resultName in names(comparisonSpecification)) {
    populations <- comparisonSpecification[[resultName]][["populations"]]
    questions <- comparisonSpecification[[resultName]][["questions"]]
    for (questionId in questions) {
      comparePopulations(questionId, populations, resultName, combinedOutputDirectory)
    }
    allQuestions <- unique(c(allQuestions, questions))
  }
  combinedStatistics <- adjustPValues(
    combineStatistics(names(comparisonSpecification), allQuestions, combinedOutputDirectory),
    adjustedField = "adjusted.p.over.combined", significantField = "adjusted.is.siginficant.combined")
  if (!is.null(combinedOutputFileName)) {
    resultFilePath <- file.path(getResultDirectory(combinedOutputDirectory), paste0(combinedOutputFileName, ".csv"))
    write.csv(combinedStatistics, resultFilePath, row.names = FALSE)
  }
  return(combinedStatistics)
}
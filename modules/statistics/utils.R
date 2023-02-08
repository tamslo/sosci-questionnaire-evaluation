#sapply(list.files("modules/data", recursive = TRUE, full.names = TRUE), source)
library(rstatix)
library(coin) # needed for effect size calculation
source("modules/config.R", local = TRUE)

getValueGroup <- function(option) {
  valuePrefixMap <- getFromConfig("valuePrefixMapping")
  valueGroup <- NULL
  for (valueKey in names(valuePrefixMap)) {
    valuePrefixes <- valuePrefixMap[[valueKey]]
    if (any(startsWith(option, valuePrefixes))) {
      valueGroup <- valueKey
      break
    }
  }
  if (is.null(valueGroup)) {
    valueGroup <- "other"
    print(paste0("[WARNING] Unhandled node group in '", option, "' (group will be 'other')"))
  }
  return(valueGroup)
}

getGroupValue <- function(group, binary = FALSE) {
  valueMap <- getFromConfig("valueMapping")
  groupValue <- valueMap[[group]]
  if (is.null(groupValue)) {
    return(NA)
  }
  if (binary) {
    if (groupValue > 0) {
      groupValue <- 1
    } else if (groupValue < 0) {
      groupValue <- -1
    }
  }
  return(groupValue)
}


getResponseValue <- function(questionId, response) {
  optionData <- getOptionData()
  questionOptions <- optionData[which(optionData$VAR == questionId),]
  responseOption <- questionOptions$MEANING[which(questionOptions$RESPONSE == response)]
  return(getGroupValue(getValueGroup(responseOption)))
}

getMean <- function(responsesList) {
  return(round(mean(responsesList), digits = 1))
}

getStatistic <- function(values, statisticFunction, responses) {
  return(normalizeValue(statisticFunction(values), responses))
}

normalizeValue <- function(value, responses) {
  possibleValues <- sort(unique(responses[responses$value > 0,]$value))
  neutralValue <- median(possibleValues)
  return(value - neutralValue)
}

runPairedWilcoxonTest <- function(firstResponses, secondResponses, alpha = 0.05) {
  confidenceLevel <- 1 - alpha
  significanceTest <- wilcox.test(firstResponses, secondResponses, paired = TRUE,
                             conf.level = confidenceLevel, alternative = "two.sided")
  testWarning <- tryCatch({
    wilcox.test(firstResponses, secondResponses, paired = TRUE,
                conf.level = confidenceLevel, alternative = "two.sided")
  }, warning = function(warningObject) {
    return(conditionMessage(warningObject))
  }, finally = function() {})
  if (typeof(testWarning) != "character") {
    testWarning <- NA
  }
  effectSize <- tryCatch({
    rstatix::wilcox_effsize(data.frame(
      "value" = c(firstResponses, secondResponses),
      "group" = c(rep(1, length(firstResponses)), rep(2, length(secondResponses)))
    ), value ~ group, paired = TRUE)
  }, error = function(errorObject) {
    return(conditionMessage(errorObject))
  }, finally = function() {})
  if (typeof(effectSize) == "character") {
    effectSizeValue <- effectSize
    effectSizeInterpretation <- NA
  } else {
    effectSizeValue <- effectSize$effsize
    effectSizeInterpretation <- effectSize$magnitude
  }
  testResults <- list(
    "pValue" = significanceTest$p.value,
    "testWarning" = testWarning,
    "effectSize" = effectSizeValue,
    "effectSizeInterpretation" = effectSizeInterpretation
  )
  return(testResults)
}

getBinaryQuestionIds <- function(options) {
  option_counts <- as.data.frame(table(options$VAR))
  names(option_counts) <- c("VAR", "COUNT")
  binary_question_ids <- as.vector(option_counts$VAR[which(option_counts$COUNT == 2)])
  for (question_id in unique(options$VAR)) {
    question_options <- options[which(options$VAR == question_id),]$MEANING
    if (("Yes" %in% question_options && "No" %in% question_options) ||
        ("Ja" %in% question_options && "Nein" %in% question_options)) {
      binary_question_ids <- c(binary_question_ids, question_id)
    }
  }
  return(binary_question_ids)
}

isComparisonBinary <- function(options, firstQuestionId, secondQuestionId) {
  binaryQuestionIds <- getBinaryQuestionIds(options)
  return(firstQuestionId %in% binaryQuestionIds && secondQuestionId %in% binaryQuestionIds)
}

runMcNemarTest <- function(firstResponses, secondResponses) {
  # Filter NA responses
  cleanPairedNaResponses <- function(responses, otherResponses) {
    getNaIndices <- function(responses) {
      return(which(is.na(responses)))
    }
    naIndices <- unique(c(getNaIndices(responses), getNaIndices(otherResponses)))
    if (length(naIndices) > 0) {
      return(responses[-naIndices])
    } else {
      return(responses)
    }
  }
  cleanFirstResponses <- cleanPairedNaResponses(firstResponses, secondResponses)
  cleanSecondResponses <- cleanPairedNaResponses(secondResponses, firstResponses)
  # Ensure square table
  allLevels <- sort(unique(c(cleanFirstResponses, cleanSecondResponses)))
  if (length(allLevels) == 1) {
    allLevels <- c(allLevels, "empty-level")
  }
  cleanFirstResponses <- factor(cleanFirstResponses, levels = allLevels)
  cleanSecondResponses <- factor(cleanSecondResponses, levels = allLevels)
  # Build contingency table
  responsePairCounts <- as.data.frame(table(cleanFirstResponses, cleanSecondResponses))
  contingencyTable <- matrix(responsePairCounts$Freq, nrow = 2)
  if (nrow(contingencyTable) != 2 || ncol(contingencyTable) != 2) {
    browser()
    print.table(contingencyTable)
    stop(paste0("Contingency table (printed above) should be 2x2!"))
  }
  # Run test
  significanceTest <- mcnemar.test(contingencyTable)
  testWarning <- tryCatch({
    mcnemar.test(contingencyTable)
  }, warning = function(warningObject) {
    return(conditionMessage(warningObject))
  }, finally = function() {})
  if (typeof(testWarning) != "character") {
    testWarning <- NA
  }
  if (!is.na(significanceTest$p.value)) {
    oddsRatio <- OddsRatio(contingencyTable, conf.level = 0.95)
    effectSize <- oddsRatio[1]
    effectSizeInterpretation <- paste0("Confidence intervals: ", oddsRatio[2], "–", oddsRatio[3])
  } else {
    effectSize <- NA
    effectSizeInterpretation <- NA
  }
  testResults <- list(
    "pValue" = significanceTest$p.value,
    "testWarning" = testWarning,
    "effectSize" = effectSize,
    "effectSizeInterpretation" = effectSizeInterpretation
  )
  return(testResults)
}

getComparisonName <- function(firstQuestionnaire, secondQuestionnaire) {
  return(paste(firstQuestionnaire, "<>", secondQuestionnaire))
}

getPairwiseComparisons <- function(questionnaires) {
  comparisons <- list()
  for (firstQuestionnaireIndex in 1:length(questionnaires)) {
    firstQuestionnaire <- questionnaires[firstQuestionnaireIndex]
    if (firstQuestionnaireIndex < length(questionnaires)) {
      for (secondQuestionnaireIndex in (firstQuestionnaireIndex + 1):length(questionnaires)) {
        secondQuestionnaire <- questionnaires[secondQuestionnaireIndex]
        comparisonName <- getComparisonName(firstQuestionnaire, secondQuestionnaire)
        comparisons[[comparisonName]] <- c(firstQuestionnaire, secondQuestionnaire)
      }
    }
  }
  return(comparisons)
}

testComparisonSignificances <- function(questionnaireQuestionList, alpha, filterParticipants) {
  questionnaires <- names(questionnaireQuestionList)
  resultData <- getResultData()
  participants <- unique(resultData$SERIAL)
  if (filterParticipants) {
    participants <- getCompleteParticipants(resultData, questionnaires)
    resultData <- resultData[resultData$SERIAL %in% participants,]
  }
  comparisons <- getPairwiseComparisons(questionnaires)
  responsesList <- list()
  questions <- getQuestions()
  for (questionniare in questionnaires) {
    questionId <- questionnaireQuestionList[[questionniare]]
    question <- questions[questions$id == questionId,]
    questionniareResults <- resultData[resultData$QUESTNNR == questionniare,]
    responses <- c()
    for (participant in participants) {
      response <- NA
      if (participant %in% questionniareResults$SERIAL) {
        participantResponse <- questionniareResults[questionniareResults$SERIAL == participant, questionId]
        if (participantResponse > 0) {
          response <- getResponseValue(questionId, participantResponse)
        }
      }
      responses <- c(responses, response)
    }
    responsesList[[questionniare]] <- responses
  }
  comparisonSignificances <- list()
  for (comparison in names(comparisons)) {
    firstQuestionnaire <- comparisons[[comparison]][1]
    secondQuestionnaire <- comparisons[[comparison]][2]
    firstResponses <- responsesList[[firstQuestionnaire]]
    secondResponses <- responsesList[[secondQuestionnaire]]
    testResults <- runPairedWilcoxonTest(firstResponses, secondResponses, alpha = alpha)
    testResults[["values"]] <- paste(
      firstQuestionnaire, ": ", paste(firstResponses, collapse = ", "), "; ",
      secondQuestionnaire, ": ", paste(secondResponses, collapse = ", ", sep = "")
    )
    comparisonSignificances[[comparison]] <- testResults
  }
  return(comparisonSignificances)
}

source("modules/data/load.R", local = TRUE)

getUniData <- function(uniKey, comparisonSpecification) {
  fullUniData <- comparisonSpecification[["by_uni"]][["populations"]][[uniKey]]
  filteredUniData <- fullUniData[,colSums(is.na(fullUniData) | fullUniData == "") < nrow(fullUniData)]
}

formatNumericValue <- function(value) {
  return(round(value, digits = 2))
}

formatEffectSize <- function(value, interpretation) {
  return(paste(formatNumericValue(value), " (", interpretation, ")", sep = ""))
}

getTableData <- function(baseQuestionIds, comparisonStats, firstGroupData, secondGroupData, latex = FALSE) {
  tableQuestions <- c()
  tableFirstGroupData <- c()
  tableSecondGroupData <- c()
  tablePValueData <- c()
  tableEffectSizeData <- c()
  newLine <- " NEWLINE "
  makeCellStart <- ""
  makeCellEnd <- ""
  if (latex) {
    newLine <- " \\newline "
    makeCellStart <- "{" # originally used "\makecell[l]" but it seems to work like this
    makeCellEnd <- "}"
  }
  questionData <- getQuestionData()
  optionData <- getOptionData()
  for (baseQuestionId in baseQuestionIds) {
    questionText <- questionData[startsWith(questionData$VAR, baseQuestionId),][1, "QUESTION"]
    questionText <- stringr::str_replace_all(questionText, "​", " ")
    tableQuestions <- c(tableQuestions, questionText)
    getQuestionAnswers <- function(responseData, baseQuestionId) {
      questionResponseColumns <- colnames(responseData)[startsWith(colnames(responseData), baseQuestionId)]
      answersString <- "–"
      if (length(questionResponseColumns) == 1) {
        questionOptions <- optionData[which(
          startsWith(optionData$VAR, baseQuestionId) & optionData$RESPONSE != -9
        ),]
        answersString <- makeCellStart
        for (rowIndex in 1:nrow(questionOptions)) {
          questionOption <- questionOptions[rowIndex,]
          optionText <- questionOption$MEANING
          if (optionText == "Other educational background") {
            optionText <- "Other"
          }
          if (optionText == "Through the course I have gained a more positive opinion on genetic analysis") {
            optionText <- "More positive"
          }
          if (optionText == "Through the course I have become more critical about genetic analysis") {
            optionText <- "More critical"
          }
          optionText <- str_replace_all(optionText, fixed("/"), "\\slash")
          optionText <- str_replace_all(optionText, fixed("WiSe"), "WT")
          optionText <- str_replace_all(optionText, fixed("SoSe"), "ST")
          answersString <- paste(answersString, optionText, ": ", sep = "")
          optionAnswers <- responseData[,questionResponseColumns]
          optionCount <- length(which(optionAnswers == questionOption$RESPONSE))
          answersString <- paste(answersString, optionCount, sep = "")
          if (rowIndex < nrow(questionOptions)) {
            answersString <- paste(answersString, newLine, sep = "")
          }
        }
        answersString <- paste(answersString, makeCellEnd, sep = "")
      } else if (length(questionResponseColumns) > 1) {
        answersString <- c()
        for (index in 1:length(questionResponseColumns)) {
          answerString <- makeCellStart
          questionResponseColumn <- questionResponseColumns[index]
          checkboxOption <- questionData[which(questionData$VAR == questionResponseColumn),]
          if (checkboxOption$TYPE != "DICHOTOMOUS") {
            next
          }
          optionText <- checkboxOption$LABEL
          optionText <- unlist(str_split(optionText, ": ", n = 2))[2]
          optionText <- substring(optionText, 1, nchar(optionText) - 1)
          if (optionText == "Analyzing genetic ancestry") {
            optionText <- "Genetic ancestry"
          }
          if (optionText == "Analyzing pharmacogenomic markers (medication effects)") {
            optionText <- "Pharmacogenomics"
          }
          if (optionText == "Analyzing wellness traits (not disease-relevant, e.g., eye color or blood type)") {
            optionText <- "Wellness traits"
          }
          if (optionText == "Carrier detection of risk variants for monogenetic diseases") {
            optionText <- "Carrier status"
          }
          if (optionText == "Analyzing risk of (common) polygenic diseases") {
            optionText <- "Polygenic diseases"
          }
          
          answerString <- paste(answerString, optionText, ": ", sep = "")
          # 1 - Not checked
          # 2 - Checked
          optionAnswers <- responseData[,questionResponseColumn]
          yesCount <- length(which(optionAnswers == 2))
          noCount <- length(which(optionAnswers == 1))
          answerString <- paste(answerString, newLine, sep = "")
          answerString <- paste(answerString, " Yes: ", yesCount, sep = "")
          answerString <- paste(answerString, newLine, sep = "")
          answerString <- paste(answerString, " No: ", noCount, sep = "")
          answerString <- paste(answerString, makeCellEnd, sep = "")
          answersString <- c(answersString, answerString)
        }
      }
      return(answersString)
    }
    getComparisonStatistics <- function(comparisonStats, baseQuestionId) {
      # If baseQuestionId is T203 choose Q410_T203 and add asterisk
      if (baseQuestionId == "T203") {
        comparisonStatistics <- getComparisonStatistics(comparisonStats, "Q410_T203")
        comparisonStatistics$p.value <- paste(comparisonStatistics$p.value, "*", sep = "")
      } else {
        questionStats <- comparisonStats[which(startsWith(comparisonStats$question.id, baseQuestionId)),]
        p.value <- c()
        effect.size <- c()
        if (nrow(questionStats) == 0) {
          p.value <- c(p.value, "–")
          effect.size <- c(effect.size, "–")
        } else {
          for (rowIndex in 1:nrow(questionStats)) {
            optionStats <- questionStats[rowIndex,]
            if (nrow(questionStats) > 1 & optionStats$question.id == baseQuestionId) {
              next
            } else {
              pValue <- formatNumericValue(optionStats$p.value)
              effectSize <- formatEffectSize(optionStats$effect.size, optionStats$strict.effect.size.interpretation)
            }
            p.value <- c(p.value, pValue)
            effect.size <- c(effect.size, effectSize)
          }
        }
        comparisonStatistics <- data.frame(p.value, effect.size)
      }
      return(comparisonStatistics)
    }
    firstGroupAnswers <- getQuestionAnswers(firstGroupData, baseQuestionId)
    secondGroupAnswers <- getQuestionAnswers(secondGroupData, baseQuestionId)
    tableFirstGroupData <- c(tableFirstGroupData, firstGroupAnswers)
    tableSecondGroupData <- c(tableSecondGroupData, secondGroupAnswers)
    comparisonStatistics <- getComparisonStatistics(comparisonStats, baseQuestionId)
    for (index in 1:nrow(comparisonStatistics)) {
      if (index > 1) {
        tableQuestions <- c(tableQuestions, "")
      }
    }
    tablePValueData <- c(tablePValueData, comparisonStatistics$p.value)
    tableEffectSizeData <- c(tableEffectSizeData, comparisonStatistics$effect.size)
    if (length(firstGroupAnswers) != length(secondGroupAnswers)) {
      higherCount <- max(length(tableFirstGroupData), length(tableSecondGroupData))
      accountForDifference <- function(list, item = "–") {
        if (length(list) < higherCount) {
          for (index in 1:(higherCount - length(list))) {
            list <- c(list, item)
          }
        }
        return(list)
      }
      tableFirstGroupData <- accountForDifference(tableFirstGroupData)
      tableSecondGroupData <- accountForDifference(tableSecondGroupData)
      tablePValueData <- accountForDifference(tablePValueData)
      tableEffectSizeData <- accountForDifference(tableEffectSizeData)
      tableQuestions <- accountForDifference(tableQuestions, item = "")
    }
  }
  
  tableColumnNames <- c(
    "Question",
    paste(makeCellStart, "HPI", newLine, "(N = ", nrow(firstGroupData), ")", makeCellEnd, sep = ""),
    paste(makeCellStart, "TUM", newLine, "(N = ", nrow(tumData), ")", makeCellEnd, sep = ""),
    paste(makeCellStart, "P-value", makeCellEnd, sep = ""),
    paste(makeCellStart, "Effect size (V)", makeCellEnd, sep = ""))
  data <- data.frame(tableQuestions, tableFirstGroupData, tableSecondGroupData, tablePValueData, tableEffectSizeData)
  colnames(data) <- tableColumnNames
  return(data)
}
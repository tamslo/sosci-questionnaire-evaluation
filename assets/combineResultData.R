library(rjson)
library(stringr)
library(textcat)

FILE_ENCODING = "UTF-16"
DATA_PREFIX = "data"
VARIABLES_PREFIX = "variables"
VALUES_PREFIX = "values"
DEFAULT_CONFIG = NULL

loadData <- function(inputDirectory, filePrefix) {
  file_pattern <- paste0(filePrefix, "(.*)csv")
  file <- list.files(inputDirectory, file_pattern, full.names = TRUE)
  data <- read.delim(file, fileEncoding = FILE_ENCODING)
  return(data)
}

getCombinationConfig <- function(outputDirectory) {
  config = DEFAULT_CONFIG
  configPath <- file.path(outputDirectory, "combination_config.json")
  if (file.exists(configPath)) {
    config <- fromJSON(file = configPath)
  }
  return(config)
}

readTable <- function(filePath) {
  return(read.delim(filePath, fileEncoding = "UTF-16"))
}

writeTable <- function(tableData, outputDirectory, fileName) {
  outputFilePath <- file.path(outputDirectory, paste0(fileName, ".csv"))
  if (file.exists(outputFilePath)) {
    file.remove(outputFilePath)
  }
  write.table(
    tableData,
    file = outputFilePath,
    fileEncoding = FILE_ENCODING,
    sep = "\t",
    row.names = FALSE,
    na = "",
    qmethod = "double"
  )
}

combineResultData <- function (inputDirectories, outputDirectory, config = DEFAULT_CONFIG, outputPostfix) {

  ### FUNCTION DEFINITIONS

  replaceQuestionId <- function(data, oldBaseQuestionId, newBaseQuestionId) {
    
    getNewQuestionId <- function(oldQuestionId, oldBaseQuestionId, newBaseQuestionId) {
      return(str_replace(oldQuestionId, oldBaseQuestionId, newBaseQuestionId))
    }
    
    if ("VAR" %in% names(data)) {
      for (rowIndex in which(startsWith(data$VAR, oldBaseQuestionId))) {
        oldQuestionId <- data[rowIndex, "VAR"]
        data[rowIndex, "VAR"] <- getNewQuestionId(oldQuestionId, oldBaseQuestionId, newBaseQuestionId)
      }
    } else {
      oldQuestionIds <- c()
      # Copy values to column with new ID
      for (nameIndex in which(startsWith(names(data), oldBaseQuestionId))) {
        oldQuestionId <- names(data)[nameIndex]
        newQuestionId <- getNewQuestionId(oldQuestionId, oldBaseQuestionId, newBaseQuestionId)
        for(rowIndex in which(!is.na(data[,oldQuestionId]))) {
          data[rowIndex, newQuestionId] <- data[rowIndex, oldQuestionId]
        }
        oldQuestionIds <- c(oldQuestionIds, oldQuestionId)
      }
      # Delete columns with old question IDs
      # Cannot do this in loop because it would shift the nameIndex
      data <- data[,!names(data) %in% oldQuestionIds]
    }
    return(data)
  }
  
  removeQuestionIds <- function(data) {
    if (!is.null(config) && "removeQuestionIds" %in% names(config)) {
      questionIds <- config[["removeQuestionIds"]]
      for (questionId in questionIds) {
        if ("VAR" %in% names(data)) {
          data <- data[which(!startsWith(data$VAR, questionId)),]
        } else {
          if (questionId %in% names(data)) {
            data <- data[,!startsWith(names(data), questionId)]
          }
        }
      }
    }
    return(data)
  }
  
  makeQuestionIdsUnique <- function(data, inputDirectory) {
    if (!is.null(config) && "makeQuestionIdsUnique" %in% names(config)) {
      questionIds <- config[["makeQuestionIdsUnique"]]
      for (questionId in names(questionIds)) {
        uniqueQuestionId <- questionIds[[questionId]][["newQuestionId"]]
        inputDirectories <- questionIds[[questionId]][["inputDirectories"]]
        if (inputDirectory %in% inputDirectories) {
          data <- replaceQuestionId(data, oldBaseQuestionId = questionId, newBaseQuestionId = uniqueQuestionId)
        }
      }
    }
    return(data)
  }
  
  normalizeQuestionIds <- function(data) {
    if (!is.null(config) && "normalizeQuestionIds" %in% names(config)) {
      questionIdMap <- config[["normalizeQuestionIds"]]
      for (questionId in names(questionIdMap)) {
        matchingQuestionIds <- questionIdMap[[questionId]]
        for (matchingQuestionId in matchingQuestionIds) {
          data <- replaceQuestionId(data, oldBaseQuestionId = matchingQuestionId, newBaseQuestionId = questionId)
        }
      }
    }
    return(data)
  }
  
  ### SCRIPT START
  
  if (is.null(config)) {
    config <- getCombinationConfig(outputDirectory)
  }
  
  if (!dir.exists(outputDirectory)) {
    dir.create(outputDirectory, showWarnings = FALSE)
  }
  
  # Combine data
  combinedData <- list(NULL, NULL, NULL)
  names(combinedData) = c(DATA_PREFIX, VARIABLES_PREFIX, VALUES_PREFIX)
  for (filePrefix in names(combinedData)) {
    singleData <- list()
    combinedDataNames <- c()
    for (inputDirectory in inputDirectories) {
      data <- makeQuestionIdsUnique(removeQuestionIds(loadData(inputDirectory, filePrefix)), inputDirectory)
      combinedDataNames <- c(combinedDataNames, names(data))
      singleData[[inputDirectory]] <- data
    }
    combinedDataNames <- unique(combinedDataNames)
    for (inputDirectory in names(singleData)) {
      data <- singleData[[inputDirectory]]
      for (name in combinedDataNames) {
        if (!name %in% colnames(data)) {
          data[[name]] <- rep(NA, nrow(data))
        }
      }
      combinedData[[filePrefix]] <- rbind(combinedData[[filePrefix]], data)
    }
  }
  
  for (filePrefix in names(combinedData)) {
    combinedData[[filePrefix]] <- normalizeQuestionIds(combinedData[[filePrefix]])
  }
  
  # Remove duplicates
  makeUnique <- list(
    list(
      "keyProperties" = c("VAR"),
      "mainTextProperty" = "QUESTION",
      "selectionFunction" = function(currentData, uniqueValue) {
        return(which(currentData$VAR == uniqueValue))
      }),
    list(
      "keyProperties" = c("VAR", "RESPONSE"),
      "mainTextProperty" = "MEANING",
      "selectionFunction" = function(currentData, uniqueValue) {
        return(which(currentData$VAR == uniqueValue$VAR & currentData$RESPONSE == uniqueValue$RESPONSE))
      })
  )
  names(makeUnique) <- c(VARIABLES_PREFIX, VALUES_PREFIX)
  
  for (uniqueFilePrefix in names(makeUnique)) {
    currentData <- combinedData[[uniqueFilePrefix]]
    keyProperties <- makeUnique[[uniqueFilePrefix]][["keyProperties"]]
    mainTextProperty <- makeUnique[[uniqueFilePrefix]][["mainTextProperty"]]
    selectionFunction <- makeUnique[[uniqueFilePrefix]][["selectionFunction"]]
    currentValues <- data.frame(currentData[, keyProperties])
    names(currentValues) <- keyProperties
    if (anyDuplicated(currentValues)) {
      uniqueValues <- unique(currentValues)
      duplicateCount <- 0
      duplicateValues <- NULL
      uniqueData <- NULL
      for (rowIndex in 1:nrow(uniqueValues)) {
        row <- uniqueValues[rowIndex,]
        dataPoints <- currentData[selectionFunction(currentData,row),]
        if (nrow(dataPoints) > 1) {
          duplicateCount <- duplicateCount + 1
          duplicateValues <- rbind(duplicateValues, dataPoints)
        }
        getEnglishIndexIfPossible <- function(mainTexts) {
          englishKey <- "english"
          germanKey <- "german"
          restrictedProfiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c(englishKey, germanKey)]
          languages <- as.vector(unlist(lapply(mainTexts, textcat, p = restrictedProfiles)))
          index <- match(englishKey, languages, nomatch = 1)
          return(index)
        }
        uniqueData <- rbind(uniqueData, dataPoints[getEnglishIndexIfPossible(dataPoints[mainTextProperty]),])
      }
      duplicate_file_path <- file.path(outputDirectory, paste0("duplicate_", uniqueFilePrefix, "_log.md"))
      print("")
      print(paste("WARNING: combined", uniqueFilePrefix, "have", duplicateCount, "non-unique values"))
      print(paste("    The duplicates are written to", duplicate_file_path))
      print("    If these duplicates are actually different instances, please include them in the combination config")
      if (file.exists(duplicate_file_path)) {
        file.remove(duplicate_file_path)
      }
      sink(duplicate_file_path)
      output <- capture.output(knitr:::print.knitr_kable(knitr::kable(duplicateValues)))
      cat(output)
      sink()
      combinedData[[uniqueFilePrefix]] <- uniqueData
    }
  }
  
  # Write to output directory
  for (filePrefix in names(combinedData)) {
    tableData <- combinedData[[filePrefix]]
    writeTable(tableData, outputDirectory, paste0(filePrefix, outputPostfix))
  }
}


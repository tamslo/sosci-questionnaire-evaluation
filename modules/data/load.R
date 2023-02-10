library(rjson)
source("scripts/adaptResultData.R", local = TRUE)
source("scripts/adaptQuestionData.R", local = TRUE)
source("scripts/adaptOptionData.R", local = TRUE)
source("modules/dataPath.R", local = TRUE)

getCsvData <- function(filePrefix, dataPath = NULL) {
  if (is.null(dataPath)) {
    dataPath <- getDataPath()
  }
  filePattern <- paste0(filePrefix, "(.*)csv")
  file <- list.files(dataPath, filePattern, full.names = TRUE)
  data <- read.delim(file, fileEncoding = "UTF-16")
  return(data)
}

getQuestionData <- function(dataPath = NULL) {
  return(adaptQuestionData(getCsvData("variables_", dataPath)))
}

getOptionData <- function(dataPath = NULL) {
  return(adaptOptionData(getCsvData("values_", dataPath)))
}

getResultData <- function(onlyFinished = TRUE, dataPath = NULL) {
  resultData <- adaptResultData(getCsvData("data_", dataPath))
  if (onlyFinished) {
    resultData <- resultData[resultData$FINISHED == 1,]
  }
  return(resultData)
}
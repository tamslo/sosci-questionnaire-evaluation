library(rjson)
source("scripts/adaptResultData.R", local = TRUE)
source("scripts/adaptQuestionData.R", local = TRUE)
source("scripts/adaptOptionData.R", local = TRUE)
source("modules/dataPath.R", local = TRUE)

getCsvData <- function(filePrefix) {
  dataPath <- getDataPath()
  filePattern <- paste0(filePrefix, "(.*)csv")
  file <- list.files(dataPath, filePattern, full.names = TRUE)
  data <- read.delim(file, fileEncoding = "UTF-16")
  return(data)
}

getQuestionData <- function() {
  return(adaptQuestionData(getCsvData("variables_")))
}

getOptionData <- function() {
  return(adaptOptionData(getCsvData("values_")))
}

getResultData <- function(onlyFinished = TRUE) {
  resultData <- adaptResultData(getCsvData("data_"))
  if (onlyFinished) {
    resultData <- resultData[resultData$FINISHED == 1,]
  }
  return(resultData)
}
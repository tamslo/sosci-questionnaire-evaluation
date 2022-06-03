#!/usr/bin/env Rscript

library(uuid)
library(stringr)

args = commandArgs(trailingOnly=TRUE)
FILE_ENCODING = "UTF-16"

anonymizeData <- function(dataPath) {

  readData <- function(dataFile) {
    return(read.delim(dataFile, fileEncoding = FILE_ENCODING))
  }

  dataFiles = list.files(dataPath, pattern = "data_(.)*.csv$", recursive = TRUE,
                         full.names = TRUE)
  serials <- c()
  for (dataFile in dataFiles) {
    data <- readData(dataFile)
    currentSerials <- data$SERIAL[which(!is.na(data$SERIAL) & data$SERIAL != "")]
    serials <- unique(c(serials, currentSerials))
  }

  serialMapping <- list()
  for (serial in serials) {
    serialMapping[serial] <- UUIDgenerate()
  }

  for (dataFile in dataFiles) {
    data <- readData(dataFile)
    for (rowIndex in 1:nrow(data)) {
      currentSerial <- data$SERIAL[rowIndex]
      if (currentSerial %in% serials) {
        serialReplacement <- serialMapping[[currentSerial]]
        data$SERIAL[rowIndex] <- serialReplacement
      }
    }
    anonymizedDataFile <- str_replace(dataFile, ".csv", "_anonymized.csv")
    write.table(
      data,
      file = anonymizedDataFile,
      fileEncoding = FILE_ENCODING,
      sep = "\t",
      row.names = FALSE,
      na = ""
    )
  }
}

if (length(args)==0) {
  stop("Please provide the data path as an argument.", call.=FALSE)
} else if (length(args)==1) {
  dataPath = args[1]
  anonymizeData(dataPath)
}
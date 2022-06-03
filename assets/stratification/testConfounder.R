testConfounderForQuestion <- function(data, questionId, answer, confounder) {
  confounderResults <- data[which(data[questionId] == answer), confounder]
  options <- optionData[optionData$VAR == confounder,]
  for (row in 1:nrow(options)) {
    option <- options[row,]
    optionResults <- confounderResults[which(confounderResults == option$RESPONSE)]
    print(paste(option$MEANING, ": ", length(optionResults), sep = ""))
  }
}
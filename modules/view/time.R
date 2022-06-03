renderTime <- function() {
  questionnaires <- getQuestionnaires()
  header <- c("")
  rows <- list()
  rows[[1]] <- c("Mean time")
  rows[[2]] <- c("Median time")
  rows[[3]] <- c("Minimum time")
  rows[[4]] <- c("Maximum time")
  for (questionnaire in questionnaires) {
    header <- c(header, questionnaire)
    time <- getTime(questionnaire)
    rows[[1]] <- c(rows[[1]], time["mean"])
    rows[[2]] <- c(rows[[2]], time["median"])
    rows[[3]] <- c(rows[[3]], time["min"])
    rows[[4]] <- c(rows[[4]], time["max"])
  }

  return(return(tagList(
    tags$h3("Time needed for completion"),
    htmlTable(header, rows)
  )))
}
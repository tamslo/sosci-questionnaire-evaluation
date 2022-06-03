testStratificationByGenotyping <- function(questionId) {
  questionResults <- tumResults[, c("Q405", questionId)]
  yesAnswers <- questionResults[questionResults[questionId] == 1,]
  noAnswers <- questionResults[questionResults[questionId] == 2,]
  print(questionId)
  print(paste("Yes:", nrow(yesAnswers[which(yesAnswers$Q405 == 1),]), "genotyped"))
  print(paste("No:", nrow(noAnswers[which(noAnswers$Q405 == 1),]), "genotyped"))
}
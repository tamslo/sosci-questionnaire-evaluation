renderQuestions <- function() {
  questions <- getQuestions()
  
  pageNames <- getFromConfig(c("replacements", "pages"))
  hiddenPages <- getFromConfig(c("hidden", "pages"))
  hiddenQuestions <- getFromConfig(c("hidden", "questions"))
  connectedQuestions <- getFromConfig("connectedQuestions")
  
  renderedPages <- c()
  currentPage <- 0
  totalBaseIds <- c()
  for (pageId in unique(questions$page)) {
    if (pageId %in% hiddenPages) {
      next
    }
    currentPage <- currentPage + 1

    renderedPage <- c()
    pageName <- applyReplacement(pageNames, pageId)
    currentContent <- 1
    renderedPage[[currentContent]] <- tags$h3(pageName)

    pageQuestions <- questions[startsWith(questions$id, pageId),]
    processedIds <- c()
    # Reorder questions according to connections
    baseQuestionIds <- c()
    for(baseQuestionId in unique(pageQuestions$base)) {
      if (!(baseQuestionId %in% totalBaseIds)) {
        hasConnection <- FALSE
        for (questionGroup in connectedQuestions) {
          if (baseQuestionId %in% questionGroup) {
            hasConnection <- TRUE
            baseQuestionIds <- c(baseQuestionIds, questionGroup)
          }
        }
        if (!hasConnection) {
          baseQuestionIds <- c(baseQuestionIds, baseQuestionId)
        }
        totalBaseIds <- c(totalBaseIds, baseQuestionIds)
      }
    }
    for (baseQuestionId in baseQuestionIds) {
      if (baseQuestionId %in% hiddenQuestions || baseQuestionId %in% processedIds) {
        next
      }
      questionParts <- questions[questions$base == baseQuestionId,]
      if (nrow(questionParts) == 0) {
        next
      }
      currentContent <- currentContent + 1
      renderedQuestion <- renderQuestion(questionParts)
      renderedPage[[currentContent]] <- renderedQuestion
    }
    renderedPages[[currentPage]] <- renderedPage
  }
  
  return(do.call(tagList, renderedPages))
}
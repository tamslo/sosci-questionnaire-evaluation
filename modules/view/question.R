renderQuestion <- function(questionParts) {
  questionType <- questionParts$type[1]
  if (questionType == "SCALE") {
    contentParts <- list()
    for (row in 1:nrow(questionParts)) {
      question <- questionParts[row,]
      contentParts <- append(
        contentParts,
        renderQuestionTabs(question)
      )
    }
    renderedQuestion <- do.call(tagList, contentParts)
  } else {
    renderedQuestion <- renderQuestionTabs(questionParts)
  }
  return(renderedQuestion)
}

renderQuestionTabs <- function(questionParts) {
  renderTabs <- function(content) {
    tabContent <- list()
    currentTab <- 0
    for (tabTitle in names(content)) {
      currentTab <- currentTab + 1
      tabContent[[currentTab]] <- list(tabTitle, content[[tabTitle]])
    }
    tabs <- lapply(tabContent, function(tabProps) { do.call(tabPanel, tabProps) })
    return(do.call(tabsetPanel, tabs))
  }
  
  questionnaires <- getQuestionnaires(questionParts[1,])
  
  renderInnerTabs <- function(question, renderFunction, questionParam, questionnaireParam) {
    allQuestionnaires <- getQuestionnaires()
    if (length(allQuestionnaires) > 1 && notAllQuestionnairesComplete()) {
      if (hasTrackedParticipants(getResultData())) {
        innerTabContent <- list()
        innerTabContent[["Complete participants"]] <- renderFunction(
          questionParam, questionnaireParam, filterParticipants = TRUE
        )
        innerTabContent[["All participants"]] <- renderFunction(
          questionParam, questionnaireParam, filterParticipants = FALSE
        )
        content <- renderTabs(innerTabContent)
      } else {
        content <- renderFunction(
          questionParam, questionnaireParam, filterParticipants = FALSE
        )
      }
    } else {
      content <- renderFunction(questionParam, questionnaireParam, filterParticipants = FALSE)
    }
    return(tags$div(content, style = "padding: 10px;"))
  }

  outerTabContent <- list()
  if (length(questionnaires) > 1) {
    baseQuestion <- questionParts[1,]
    questionType <- baseQuestion$type
    if (questionType == "SCALE") {
      outerTabContent[["All questionniares"]] <- renderInnerTabs(
        baseQuestion, renderScaleComparison, questionParts, questionnaires
      )
    }
    if (questionType == "SELECTION") {
      outerTabContent[["All questionniares"]] <- renderInnerTabs(
        baseQuestion, renderSelectionComparison, questionParts, questionnaires
      )
    }
  }
  for (questionnaire in questionnaires) {
    outerTabContent[[questionnaire]] <- renderInnerTabs(
      baseQuestion, renderSingleQuestion, questionParts, questionnaire
    )
  }
  return(tagList(
    tags$br(),
    renderQuestionTitle(questionParts),
    renderTabs(outerTabContent),
    tags$br()
  ))
}

renderQuestionTitle <- function(questionParts) {
  questionType <- questionParts$type[1]
  questionText <- questionParts$text[1]
  questionOption <- questionParts$option[1]
  questionId <- paste("[", questionParts$base[1], "] ", sep = "")
  title <- tagList(tags$span(questionId, style = "color:gray;"), tags$strong(HTML(questionText)), tags$br())
  if (questionType == "SCALE") {
    title <- tagList(
      title,
      subSectionHeading(questionOption)
    )
  }
  return(tagList(title, tags$br()))
}

renderSingleQuestion <- function(questionParts, questionnaire, filterParticipants) {
  questionType <- questionParts$type[1]
  if (questionType == "OPEN") {
    renderedQuestion <- renderOpenQuestion(questionParts, questionnaire, filterParticipants)
  } else if (questionType == "SELECTION") {
    renderedQuestion <- renderSelectionQuestion(questionParts, questionnaire, filterParticipants)
  } else if (questionType == "SCALE") {
    renderedQuestion <- renderScaleQuestion(questionParts, questionnaire, filterParticipants)
  } else if (questionType == "SYSTEM") {
    renderedQuestion <- renderCheckboxQuestion(questionParts, questionnaire, filterParticipants)
  } else {
    renderedQuestion <- tags$p(
      style = "color:red",
      paste("Cannot render question; unknown type:", questionType)
    )
  }
  return(renderedQuestion)
}
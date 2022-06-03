getOpenContent <- function(question, questionnaire, filterParticipants) {
  htmlList <- function(responses) {
    list <- "<ul>"
    for (text in responses) {
      list <- paste(list, "<li>", text, "</li>", sep = "")
    }
    list <- paste(list, "</ul>", sep = "")
    return(HTML(list))
  }
  
  replacements <- getFromConfig(c("replacements", "answers"))
  hasReplacement <- FALSE
  if (question$id %in% names(replacements)) {
    possibleReplacements <- replacements[[question$id]]
    for (possibleReplacement in possibleReplacements) {
      if (questionnaire %in% possibleReplacement$questionnaires) {
        hasReplacement <- TRUE
        content <- HTML(possibleReplacement$text)
        break
      }
    }
  }
  
  if (!hasReplacement) {
    allResponses <- getResponses(question, questionnaire, filterParticipants)
    responses <- allResponses[which(!is.na(allResponses))]
    content <- htmlList(responses)
  }
  return(content)
}

renderOpenQuestion <- function(questionParts, questionnaire, filterParticipants) {
  baseQuestion <- questionParts[1,]
  return(tagList(tags$br(), getOpenContent(baseQuestion, questionnaire, filterParticipants)))
}
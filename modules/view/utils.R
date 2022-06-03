subSectionText <- function(text) {
  return(HTML(paste("<p style='font-size:0.9em'>", text, "</p>", sep = "")))
}

subSectionHeading <- function(text) {
  return(subSectionText(paste("<strong>&#9659; ", text, "</strong>", sep = "")))
}

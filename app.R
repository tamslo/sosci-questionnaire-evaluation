# Load all required packages and modules once; this is probably not a smart
# thing to do but R does not seem to mind if imports are missing in subsequent
# files anyways and this seems more clean to me.
# In the future use proper modules!
library(rjson)
library(shiny)
library(shinycssloaders)
library(stringr)
sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  uiOutput("title"),
  uiOutput("time"),
  withSpinner(uiOutput("questions"), type = 7)
)

server <- function(input, output) {
  output$title <- renderUI({  renderTitle() })
  output$time <- renderUI({ renderTime() })
  output$questions <- renderUI({ renderQuestions() })
}

shinyApp(ui, server, options = list(host = '0.0.0.0', port = 8888))
library(stringr)

getPlotId <- function(question, questionnaire, filterParticipants) {
  postfix <- ""
  if (filterParticipants) {
    postfix <- "_filtered"
  }
  return(paste(questionnaire, "_", question$id, postfix, sep = ""))
}

getImageDirectory <- function() {
  return("images")
}

getImagePath <- function(plotId) {
  return(file.path(getImageDirectory(), paste(plotId, ".png", sep = "")))
}

getPlotPath <- function(plotId) {
  projectPrefix <- file.path(getwd(), "www")
  imageDirectory <- file.path(projectPrefix, getImageDirectory())
  if (!dir.exists(imageDirectory)) {
    dir.create(imageDirectory)
  }
  return(file.path(projectPrefix, getImagePath(plotId)))
}

renderPlotImage <- function(plotId) {
  return(tags$img(src = getImagePath(plotId), alt = paste("Plot for", plotId)))
  # return(renderImage({
  #   list(src = plotPath, alt = paste("Plot for", plotId))
  # }, deleteFile = FALSE))
}

# width: 780 defined in CSS
createPng <- function(plotFunction, plotParams, plotPath, width = 780, height = 400, pointsize = 12) {
  imageParams <- list(
    "filename" = plotPath,
    "width" = width,
    "height" = height,
    "unit" = "px",
    "pointsize" = pointsize
  )
  do.call(png, imageParams)
  plotFunction(plotParams)
  dev.off()
}

createPdf <- function(plotFunction, plotParams, plotPath, width = 30, height = 12, pointsize = 12) {
  pdfParams <- list(
    "file" = plotPath,
    "width" = width,
    "height" = height,
    "pointsize" = pointsize
  )
  do.call(pdf, pdfParams)
  plotFunction(plotParams)
  dev.off()
}

renderPlot <- function(question, questionnaire, plotFunction, plotParams, filterParticipants) {
  plotId <- getPlotId(question, questionnaire, filterParticipants)
  plotPath <- getPlotPath(plotId)
  overrideImages <- getFromConfig("overrideImages")
  if (overrideImages || !file.exists(plotPath)) {
    createPng(plotFunction, plotParams, plotPath)
  }
  return(renderPlotImage(plotId))
}
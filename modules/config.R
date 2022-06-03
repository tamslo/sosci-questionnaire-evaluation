getFromConfig <- function(path) {
  configFilePath <- "config.json"
  config <- fromJSON(file = configFilePath)
  result <- config
  for (key in path) {
    result <- result[[key]]
  }
  return(result)
}

applyReplacement <- function(replacements, label) {
  for (replacement in names(replacements)) {
    if (label == replacement) {
      label <- replacements[[replacement]]
      break
    }
  }
  return(label)
}
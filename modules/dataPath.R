library(rjson)

getDataPath <- function() {
  return(fromJSON(file = "data_path.json")[["path"]])
}
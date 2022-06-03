htmlTable <- function(header, rows, tableContainerStyle = NA) {
  tableRow <- function(fields, header = FALSE) {
    row <- "<tr>"
    fieldIndex <- 0
    for (field in fields) {
      fieldIndex <- fieldIndex + 1
      if (header || fieldIndex == 1) {
        tagName <- "th"
      } else {
        tagName <- "td"
      }
      cellStyle <- "border-bottom: 1px solid lightgrey;"
      cellStyle = paste(cellStyle, "padding: 5px; text-align: center;", sep = "")
      cellStyle = paste(cellStyle, "width: ", 100 / length(fields), "%;", sep = "")
      cell <- paste("<", tagName, " style='", cellStyle, "'>", field, "</", tagName, ">", sep = "")
      row <- paste(row, cell, sep = "")
    }
    return(paste(row, "</tr>", sep = ""))
  }
  tableText <- "<table style='width: 100%;'>"
  tableText <- paste(tableText, tableRow(header, header = TRUE), sep = "")
  for (row in rows) {
    tableText <- paste(tableText, tableRow(row), sep = "")
  }
  if (is.na(tableContainerStyle)) {
    tableContainerStyle <- "width: 100%; padding-left: 10px; padding-right: 10px"
  }
  return(tags$div(HTML(paste(tableText, "</table>", sep = "")), style = tableContainerStyle))
}
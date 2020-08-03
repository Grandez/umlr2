#' Read a text file containing an UML definition according PlantUML tool
#'
#' @param fileName The file to read
#' @return An PlantUML S3 object
#' @export
readPlantUML = function(fileName) {
  data = readtext(fileName)
  data$class = "PlantUML"
}

#' Convert data, if possible, to a suitable PlantUML S3 Object
#'
#' @details Throw an plantUMLErr expection
#' @param uml Data convertible to an String with a plantuml definition
#' @return An PlantUML S3 object
#' @export
as.plantuml = function(uml) {
  txt = data
  if (is.list(uml)) txt = unlist(uml)
  if (!is.character(txt)) {
    p = PLANTUML$new()
    p$launchException("RXXXX")
  }
  if (length(txt) > 1) txt = paste(txt, collapse="\n")
  txt
}

#' plot a diagram definition using PlantUML package
#'
#' @param uml A file or string containing the UML definition
#'            attribute class must set to PlantUML
#' @export
plot.PlantUML = function(uml) {
   p = PLANTUML$new()
   p$plot(uml)
}

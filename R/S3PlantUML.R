#' Read a text file containing an UML definition according PlantUML tool
#'
#' @param fileName The file to read
#' @return A PlantUML object
#' @export
readPlantUML = function(fileName=NULL) {
   PLANTUML$new()$readPlantUML(fileName)
}

#' Convert data, if possible, to a suitable PlantUML S3 Object
#'
#' @details Throw an plantUMLErr expection
#' @param uml Data convertible to an String with a plantuml definition
#' @return An PlantUML S3 object
#' @export
as.plantuml = function(uml) {
  PLANTUML$new()$asS3PlantUML(uml)
}

#' plot a diagram definition using PlantUML package
#' @family generics
#' @param uml A file or string containing the UML definition
#'            attribute class must set to PlantUML
#' @method plot S3PlantUML
plot.S3PlantUML = function(uml) {
   PLANTUML$new()$plot(uml)
}

plot <- function(uml) UseMethod("plot")


#' La clase para interactuar con plantuml
#' @title Template
#' @name R6TEMPLATE
#' @aliases TEMPLATEL
#' @docType class
#' @description  La descripcion.
TEMPLATE = R6::R6Class("R6TEMPLATE"
   ,public = list(
      add = function (template) {
         private$tplFiles = c(private$tplFiles, template)
         invisible(self)
      }
      #' @description Crea un fichero temporal con el diagrama
      #'              generado a partir de los datos pasados
      #' @param data  Un objeto S3PlantUML o un vector de lineas interpretables por PlantUML
      #' @return El nombre del fichero con el diagrama
      ,generate      = function(summary) {
          vars = VARS$new()
          skinparam = readLines(private$tplFiles[[1]])

          skinparam = private$process(skinparam, summary[["subclasses"]], vars$subClass)
          skinparam = private$process(skinparam, summary[["superclasses"]], vars$superClass)
          skinparam = private$process(skinparam, summary[["classes"]], vars$class)

          skinparam[grep(vars$pattern, skinparam, invert=TRUE)]
      }
   )
   ,private = list(
       #tplFiles  = list(system.file("extdata", "default.tpl", package = "UMLR2"))
      # Falla en desarrollo
      tplFiles  = list("P:/R/umlr2/inst/extdata/default.tpl")
      ,process = function(skinparam, classes, tag) {
         if (is.null(classes)) return (skinparam)
         c(sapply(classes, function(x) gsub(tag, x, skinparam, ignore.case=TRUE)))
      }
   )
)

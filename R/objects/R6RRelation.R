#' La clase para interactuar con plantuml
#' @title "R6 Class"
#' @name "R6 Class"
#' @rdname R6Class
#' @docType class
#' @description  La descripcion.
RRelation = R6::R6Class("RRELATION"
   ,inherit      = RComponent
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       #' @field generator Generador de la clase
        from = NULL
       #' @field detail Nivel de detalle
       ,to      = NULL
       #' @field deep Nivel de profundidad
       #' @field type Tipo de clase (main, super, sub) 1 - 2 - 4

       #' @description Crea una instancia de la clase
       #' @param name  Nombre de la clase
       #' @param generator Objeto generados
       #' @param detail Nivel de detalle generado
       #' @param deep   Nivel de profundidad
       #' @param type   Tipo de clase
       ,initialize = function(from, to, type) {
           super$initialize(ObjType$relation, paste(from,to,sep="-"),type)
           self$from  = from
           self$to    = to
       }
      #' @description Devuelve la definicion de la clase en formato S3PlantUML
      #' @param detail  Nivel de detalle deseado
      #' @return La definicion de la clase en formato S3PlantUML
      ,definition = function() {
          paste(from, toValue(toName(type, RelType), lang), to)
      }
    )
    ,private = list(
    )
)

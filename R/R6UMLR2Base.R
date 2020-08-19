#' @name UMLR2Base
#' @title UMLR2Base
#' @rdname R6UMLR2BASE
#' @docType class
#' @description  Clase base del paquete.
#'               Contiene los mensajes de error.
#' @import R6
library(R6)
UMLR2Base = R6Class("R6UMLR2BASE"
   ,public = list(
       #' @description Inicializador
       initialize = function(...) {
          if (substr(as.character(sys.call(-1))[1], 1, 9) == "UMLR2Base") private$plantErr("E900")
          private$cfg = CONFIG$new(...)
       }
       #' @description Cambia los datos de configuracion de la instancia
       #' @param ...  named values para definir la configuracion
       #' @return La instancia del objeto
       ,setConfig         = function(...) {
          private$cfg$setConfig(...)
          invisible(self)
       }
       #' @description Obtiene los datos de configuracion
       #' @return Una lista con los datos de configuracion
       ,getConfig         = function() { private$cfg }

    )
  ,private = list(S3Class = "S3PlantUML"
      ,msg = UMLR2Msg.getInstance()
      ,cfg = CONFIG$new()
   )
)

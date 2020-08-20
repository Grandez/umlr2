#' @name UMLR2Base
#' @title UMLR2Base
#' @rdname R6UMLR2BASE
#' @docType class
#' @description  Clase base del paquete.
#'               Contiene los mensajes de error.
#' @import R6
library(R6)
UMLR2BASE = R6Class("R6UMLR2BASE"
   ,public = list(
       #' @description Inicializador
       initialize = function(...) {
          if (substr(as.character(sys.call(-1))[1], 1, 9) == "UMLR2BASE") private$plantErr("E900", "UMLR2BASE")
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
       ,getConfig          = function() { private$cfg }
       ,checkConfiguration = function(verbose=TRUE, first=FALSE) { private$cfg$checkConfiguration(verbose, first) }
       ,checkInstallation  = function(verbose=TRUE, first=FALSE) { private$cfg$checkInstallation (verbose, first) }
    )
  ,private = list(S3Class = "S3PlantUML"
      ,msg = UMLR2Msg.getInstance()
      ,cfg = CONFIG$new()
   )
)

#' @name UMLR2Base
#' @title UMLR2Base
#' @rdname R6UMLR2BASE
#' @docType class
#' @description  Clase base del paquete.
UMLR2Base = R6::R6Class("R6UMLR2BASE"
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       #' @description Inicializador
       #' @param ... datos para configuracion
        initialize = function(...) {
#          if (substr(as.character(sys.call(-1))[1], 1, 9) == "UMLR2BASE") msg$err("E900", "UMLR2BASE")
          cfg$setConfig(...)
       }
       #' @description Cambia los datos de configuracion de la instancia
       #' @param ...  named values para definir la configuracion
       #' @return La instancia del objeto
       ,setConfig         = function(...) {
          cfg$setConfig(...)
          invisible(self)
       }
       #' @description Obtiene los datos de configuracion
       #' @return Una lista con los datos de configuracion
       ,getConfig          = function() { cfg }
       #' @description Chequea si la configuracion es correcta
       #' @details Esta funcion no verifica que los datos sean reales.
       #'          Para verificar completamente el sistema usar checkInstallation
       #' @param verbose Muestra mensajes informativos
       #' @param first   Detiene el proceso en el primer error (si lo hay)
       #' @return TRUE si lo es
       #'         FALSE si no
       ,checkConfiguration = function(verbose=TRUE, first=FALSE) { cfg$checkConfiguration(verbose, first) }
       #' @description Chequea si la configuracion y las dependencias son correctas y estan disponibles
       #' @param verbose Muestra mensajes informativos
       #' @param first   Detiene el proceso en el primer error (si lo hay)
       #' @return TRUE si lo es
       #'         FALSE si no
       ,checkInstallation  = function(verbose=TRUE, first=FALSE) { cfg$checkInstallation (verbose, first) }
    )
  ,private = list(S3Class = "S3UMLR2"
      ,cfg = CONFIG$new()
      ,msg = UMLR2MSG$new()
   )
)

#' Clase generica para identificar objetos del paquete
#  Mantenemos el sufijo 2 para que no se "pegue" con el paquete standalone
#' @title UMLR2
#' @name UMLR2
#' @rdname R6UMLR2
#' @docType class
#' @description  La descripcion.
#' @export
UMLR2 = R6::R6Class("UMLR2"
   ,inherit      = UMLR2Base
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       initialize = function(...)  {
          super$initialize(...)
          # parms= list(...)
          # if (length(parms) > 0) private$cfg = CONFIG$new(parms)
       }
       ,finalize = function() {

       }
       # ,setConfig = function(config) {
       #    private$cfg = config()
       # }
       ,print = function() {
          message("Soy una clase UMLR2")
          invisible(self)
       }
        ,test = function() {
          msg$msg("I000")
       }
    )
   ,private = list(
      #  msg = UMLR2MSG$new() # umlr2.env$UMLR2Msg
      # ,cfg = CONFIG$new()
    )
)


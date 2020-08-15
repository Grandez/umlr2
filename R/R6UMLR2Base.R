#' Clase base
#' @title UML2RBase
#' @name UMLR2Base
#' @aliases R6UMLR2Base
#' @docType class
#' @description  Contiene los mensajes de error
#' @import R6
UMLR2Base = R6::R6Class("R6UMLR2Base"
   ,public = list(
       #' @description Inicializador
       initialize = function() {
          if (substr(as.character(sys.call(-1))[1], 1, 6) == "PLANTUML") private$plantErr("E900")
       }
    )
  ,private = list(
      msg = UMLR2Msg.getInstance()
   )
)

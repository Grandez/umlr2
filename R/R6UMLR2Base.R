#' Clase base
#' @title UML2RBase
#' @name UMLR2Base
#' @aliases R6UMLR2Base
#' @docType class
#' @description  Contiene los mensajes de error
library(R6)
UMLR2Base = R6::R6Class("R6UMLR2Base"
   ,public = list(
       #' @description Inicializador
       initialize = function() {
          if (substr(as.character(sys.call(-1))[1], 1, 9) == "UMLR2Base") private$plantErr("E900")
       }
    )
  ,private = list(
      msg = UMLR2Msg.getInstance()
   )
)

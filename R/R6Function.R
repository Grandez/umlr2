#' Funcion
#' @title RFunction
#' @docType class

RFunction = R6::R6Class("R6Function", inherit = RObject,
   public = list(
        initialize = function(name, `signature`) {
           self$name = name
           self$type = RType$method
           if (!missing(`signature`)) {
               cc = as.character(`signature`)
               self$signature = ifelse (substr(cc,1,1) == "(", substr(cc,2,nchar(cc) - 1), cc)
           }
        }
       ,signature = ""
   )
)

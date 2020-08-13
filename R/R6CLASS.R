R6CLASS = R6::R6Class("R6CLASS",
    public = list(
        name = NULL
       ,generator = NULL
       ,deep      = 0
       ,initialize = function(name, generator, deep) {
           self$name = name
           self$generator = generator
           self$deep = deep
       }
      ,addParent = function (parent) {
          private$parents[[length(private$parents) + 1]] = parent
          invisible(self)
      }
      ,addFields = function(fields, public) {
        if (public)  private$publicFields  = fields
        if (!public) private$privateFields = fields
        invisible(self)
      }
      ,addMethods = function(methods, public) {
        methods["initialize"] = NULL
        methods["finalize"]   = NULL
        methods["clone"]      = NULL

        if (public)  {
           # Separar getters y setters
           private$publicMethods  = methods
        }
        if (!public) private$privateMethods  = methods
        invisible(self)
      }
      ,addBindings = function(binds) {
         private$binds = names(binds)
         invisible(self)
      }
      ,addComposition = function(hijos) {
        private$composition = c(private$composition, hijos)
        invisible(self)
      }
      ,addAggregation = function(hijos) {
        private$aggregated = hijos
        invisible(self)
      }
      ,getClassDefinition = function(detail) {
          privF = NULL
          privM = NULL
          cdef = c(paste("class", self$name, "<<", self$generator, ">> {")
                   ,private$getFieldsDefinition(TRUE)
                   ,private$getBindings()
          )
          pubM = private$getMethodsDefinition(TRUE)
          if (bitwAnd(detail,UMLType$complete) > 0) {
              privF = private$getFieldsDefinition(FALSE)
              privM = private$getMethodsDefinition(FALSE)
          }

          c(cdef, privF, pubM, privM, "}")
      }
      ,getParentsRelation = function() {
          if (length(private$parents) == 0) return ("")
          unlist(lapply(seq(1,length(private$parents)), function(x) paste(private$parents[[x]]$name, "<|--", self$name)))
      }
      ,getSubclassesRelation = function() {
          data = c()
          if (length(private$composition) > 0) {
              data = unlist(lapply(seq(1,length(private$composition)),
                                   function(x) paste(self$name, "*--", private$composition[[x]]$name)))
          }
          if (length(private$aggregated) > 0) {
              data = c(data, unlist(lapply(seq(1,length(private$aggregated)),
                                           function(x) paste(self$name, "o--", private$aggregated[[x]]$name))))
          }
          data
      }
    )
    ,private = list(
         detail = 254 # .UMLRDefs$new()
        ,parents = list()
        ,publicFields   = NULL
        ,publicMethods  = NULL
        ,privateFields  = NULL
        ,privateMethods = NULL
        ,binds          = NULL
        ,accesors       = NULL
        ,composition    = NULL
        ,aggregated     = NULL
        ,.attrs  = function(src, prefix) {
            if (is.null(src)) return (NULL)
            unlist(lapply(src, function(x) paste(prefix, x)))
        }
        ,getFieldsDefinition = function(public) {
            a = paste0(ifelse(public, "+", "-"),"{field}")
            fields = private$privateFields
            if (public) fields = private$publicFields
            private$.attrs(names(fields), a)
        }
        ,getMethodsDefinition = function(public) {
            a = paste0(ifelse(public, "+", "-"),"{method}")
            methods = private$privateMethods
            if (public) methods = private$publicMethods
            private$.attrs(names(methods), a)
        }
        ,getBindings          = function() {
            if (is.null(private$binds)) return ("")
            #c(".. Active bindings ..",
              unlist(lapply(private$binds, function(x) paste0("#//",x, "//")))
          #)
        }

    )
)

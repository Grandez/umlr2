#' Encapsula la informacion de un objeto de tipo R6
#' @title R6CLASS
#' @docType class
R6CLASS = R6::R6Class("R6CLASS",
    public = list(
        #' @field name Nombre de la clase
        name = NULL
        #' @field generator Generador de la clase
       ,generator = NULL
        #' @field deep Nivel de profundidad en el analisis
       ,deep      = 0
       #' @description Crea una instancia de la clase
       #' @param name  Nombre de la clase
       #' @param generator Objeto generados
       #' @param deep Nivel de profundidad
       #' @return La instancia del objeto
       ,initialize = function(name, generator, deep) {
           self$name = name
           self$generator = generator
           self$deep = deep
       }
       #' @description Agrega el padre (superclase)
       #' @param parent  Superclase
      ,addParent = function (parent) {
          private$parents[[length(private$parents) + 1]] = parent
          invisible(self)
      }
      #' @description Agrega los atributos de la clase
      #' @param fields  Los atributos a agregar
      #' @param public TRUE si los atributos son publicos
      ,addFields = function(fields, public) {
         if (public)  private$publicFields  = fields
         if (!public) private$privateFields = fields
         invisible(self)
      }
      #' @description Agrega los metodos de la clase
      #' @param methods  Los metodos a agregar
      #' @param public TRUE si los metodos son publicos
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
      #' @description Agrega los _active bindings_ de la clase
      #' @param binds  _active bindings_
      ,addBindings = function(binds) {
         private$binds = names(binds)
         invisible(self)
      }
      #' @description Agrega subclases a la clase
      #' @param subclasses  subclases que son parte de la clase
      #' @param composition TRUE si son parte de ella
      #'                    FALSE si son usadas dentro de ella
      ,addSubclasses = function(subclasses, composition) {
          if( composition) private$composition = c(private$composition, subclasses)
          if(!composition) private$aggregated  = c(private$aggregated, subclasses)
          invisible(self)
      }
      #' @description Devuelve la definicion de la clase en formato S3PlantUML
      #' @param detail  Nivel de detalle deseado
      #' @return La definicion de la clase en formato S3PlantUML
      ,getClassDefinition = function(detail) {
          privF = NULL
          privM = NULL
          cdef = c(paste("class", self$name, "<<", self$generator, ">> {")
                   ,private$getFieldsDefinition(TRUE)
                   ,private$getBindings()
          )
          pubM = private$getMethodsDefinition(TRUE)
          if (bitwAnd(detail,UMLShow$complete) > 0) {
              privF = private$getFieldsDefinition(FALSE)
              privM = private$getMethodsDefinition(FALSE)
          }
          c(cdef, privF, pubM, privM, "}")
      }
      #' @description Devuelve la relacion de herencia si existe
      #' @return La relacion de herencia si existe
      ,getParentsRelation = function() {
          if (length(private$parents) == 0) return ("")
          unlist(lapply(seq(1,length(private$parents)), function(x) paste(private$parents[[x]]$name, "<|--", self$name)))
      }
      #' @description Devuelve la relacion de subclases si existen
      #' @return La relacion de subclases si existen
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

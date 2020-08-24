#' La clase para interactuar con plantuml
#' @title "R6 Class"
#' @name "R6 Class"
#' @rdname R6Class
#' @docType class
#' @description  La descripcion.
RClass = R6::R6Class("RCLASS"
   ,inherit      = RComponent
   ,portable     = FALSE
   ,lock_objects = TRUE
   ,lock_class   = TRUE
   ,public = list(
       #' @field generator Generador de la clase
        generator = NULL
       #' @field detail Nivel de detalle
       ,detail      = 0
       #' @field deep Nivel de profundidad
       # Necesario para evitar la recursividad
       ,deep  = 0
       #' @field type Tipo de clase (main, super, sub) 1 - 2 - 4
       ,type = 0
       #' @description Crea una instancia de la clase
       #' @param name  Nombre de la clase
       #' @param generator Objeto generados
       #' @param detail Nivel de detalle generado
       #' @param deep   Nivel de profundidad
       #' @param type   Tipo de clase
       ,initialize = function(name, generator, detail, deep, type = ClassType$unknow) {
           self$name      = name
           self$generator = generator
           self$detail    = detail
           self$deep      = deep
           self$type      = type
       }
       #' @description Establece el tipo de clase dentro de su contexto
       #' @details El contexto indica si se ha analizado como clase heredada, clase padre, etc.
       #' @param t  El tipo de la clase
       ,setType = function(t) {
         self$type = bitwOr(self$type, t)
       }
       #' @description Verifica si es una clase principal
       #' @return TRUE si lo es
       #'         FALSE si no
       ,isClass      = function() as.logical(bitwAnd(self$type, ClassType$class))
       #' @description Verifica si tiene activo el flag de superclase
       #' @return TRUE si lo es
       #'         FALSE si no
       ,isSuperclass = function() as.logical(bitwAnd(self$type, ClassType$superClass))
       #' @description Verifica si tiene activo el flag de subclase
       #' @return TRUE si lo es
       #'         FALSE si no
       ,isSubclass   = function() as.logical(bitwAnd(self$type, ClassType$subClass))
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
         parents = list()
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

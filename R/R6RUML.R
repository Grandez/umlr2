#' La clase para interactuar con plantuml
#' @title R6UMLR
#' @docType class
#' @description  La descripcion.
#' @import R6
#' @export
# #' @import magick
#'
library(R6)
UMLR = R6::R6Class("UMLR", inherit = PLANTUML,
                       public = list(
                           #####
                           #' @description Crea una instancia de la clase PLANTUML
                           #' @param ...  named values para definir la configuración
                           #' @return La instancia del objeto
                           #'
                           #' @examples
                           #' plant = PLANTUML$new()
                           #' plant = PLANTUML$new(jvm='java')
                           #' plant = PLANTUML$new(c(jvm='java', plantuml='plantuml.jar'))
                           ,initialize         = function( ...) {
                               parms = unlist(list(...))
                               if (sum(names(parms) == "") > 0) private$plantErr("R103")
                               self$setConfig(parms)
                           }
                           ,finalize = function() {
                               message("Cleaning up ")
                           }
                           #' @description Genera un diagrama a partir de la definición pasada
                           #' @param data  Definición del diagrama o fichero con la misma
                           #' @details
                           #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
                           #'     - El fichero con la imagen no se almacena en el sistema de archivos
                           #' @description Genera el diagrama de la clase pasada
                           ,plotClass             = function (object, full=FALSE, deep = FALSE) {
                               self$plot(self$umlClass(object,full, deep))
                           }
                           ,umlClass             = function (object, full=FALSE, deep = FALSE) {
                               if (isS4(object))            return (private$parseS4(object, full, deep))
                               if ("R6" %in% class(object)) return (private$parseR6(object, full, deep))
                               warning("'object' is not an instance of S4 or R6 Classes"))
                           }
)
,private = list(
    ,parseS4 = function (object, full=FALSE, deep = FALSE) {
    }
    ,parseR6 = function (object, full=FALSE, deep = FALSE) {
        #uml = ""
        lista = list()
        defs = private$getGenerators()  # Generadores
        classes = class(object)
        classes = classes[classes != "R6"]
        inherits = private$getExtends(classes, defs, deep)
        classBase = private$UMLClass(classes[1], full, defs)
        if (deep && length(classes) > 1) {
            lista = sapply(classes[-1], function(x) private$UMLClass(x, full, defs))
        }
        #uml = c(uml, paste("class", class(object)[1], "{"))
        #data = capture.output(object)
        #data = trimws(data)
        c(classBase, unlist(lista), inherits)
    }
    ,getGenerators = function () {
        vars = ls(globalenv())
        classes = lapply(vars, function(x) eval(parse(text=paste0("class(",x,")"))))
        names(classes) = vars
        classes = unlist(classes)
        c2 = classes[classes == "R6ClassGenerator"]
        # Obtenemos la clase que genera
        c2 = sapply(names(c2), function(x) eval(parse(text=paste0(x, "$classname"))), USE.NAMES=T)
        # Cambiamos nombres por valores
        nm = names(c2)
        names(nm) = c2
        nm
    }
    ,getExtends        = function (classes, defs, deep) {
        if (length(classes) == 1) return ("")
        cNames = defs[classes]
        extend = c(paste(cNames[classes[2]], "<|--", cNames[classes[1]]))
        if (deep && length(classes) > 2) {
            rr = sapply(seq(2, length(classes) - 1), function(x)
                c(paste(cNames[classes[x+1]], "<|--", cNames[classes[x]])))
        }
        # if (!deep && length(classes) > 1) return (c(paste(classes[1], "<|--", classes[2])))
        # for (i in 1:length(classes) - 1) {
        #     extend = c(extend, paste(classes[i], "<|--", classes[i+1]))
        # }
        extend
    }
    ,UMLClass          = function (className, full, generators) {
        attrPrivate = NULL
        uml = c(paste("class", generators[className], "<<", className, ">> {"))
        attrPublic  = private$getAttrs(generators[className], "public")
        if (full) attrPrivate  = private$getAttrs(generators[className], "private")
        if (!is.null(attrPublic$fields))   uml = c(uml, paste("+{field}",  attrPublic$fields))
        if (!is.null(attrPrivate$fields))  uml = c(uml, paste("-{field}",  attrPrivate$fields))
        if (!is.null(attrPublic$methods))  uml = c(uml, paste("+{method}", attrPublic$methods))
        if (!is.null(attrPrivate$methods)) uml = c(uml, paste("-{method}", attrPrivate$methods))
        c(uml, "}")
    }
    ,getAttrs  = function (className, visibility) {
        data = list(fields = NULL, methods = NULL)
        lFields  = eval(parse(text=paste0(className, "$", visibility, "_fields")))
        lMethods = eval(parse(text=paste0(className, "$", visibility, "_methods")))
        list(fields=names(lFields), methods=names(lMethods))
    }
    ,extract          = function(data, type) {
        attrs = grep("function", data, fixed=TRUE, value=TRUE, invert=TRUE)
        attrs = gsub(":.*", "", attrs)

        defs = grep("function", data, fixed=TRUE, value=TRUE)
        defs = gsub(": function", "", defs)
        c(paste0(type, attrs), "--", paste0(type, defs))
    }
    ,parseR6Parent    = function(txtParent) {
        parent = trimws(gsub(">", "", gsub(".*<", "", txtParent)))
        c(paste("class", parent, "{"), "}")
        vars = ls()
        classes = lapply(vars, function(x) eval(parse(text=paste0("class(",x,")"))))
        names(classes) = vars

    }
    ,removeUmlTags   = function (data) {
        # Es mas rapido obtener el indice que hacer una sustitucion
        pos = grep("@startuml", data)
        if (pos > 0) data[pos] = ""
        pos = grep("@enduml", data)
        if (pos > 0) data[pos] = ""
        data
    }
)
)

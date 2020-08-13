#' La clase para interactuar con plantuml
#' @title R6PlantUML
#' @docType class
#' @description  La descripcion.
#' @import R6
#' @export
# #' @import magick
#'
library(R6)
UML = R6::R6Class("R6UML", inherit = PLANTUML,
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
                            initialize         = function( ...) {
                               super$initialize(...)
                           }
                           ,finalize = function() {
                               super$finalize()
                               message("Cleaning up R6UML")
                           }
                           #' @description Genera un diagrama a partir de la definición pasada
                           #' @param data  Definición del diagrama o fichero con la misma
                           #' @details
                           #'     - Si no se especifica type se asume el tipo de imagen definido en la instancia
                           #'     - El fichero con la imagen no se almacena en el sistema de archivos
                           ,plot               = function(data=NULL, force) {
                               ff = ifelse (missing(force), self$getForce(), force)
                               imgFile = private$makeImage(data, ff)
                               knitr::include_graphics(imgFile)
                           }
                           #' @description Genera un link al fichero de imagen con el diagrama
                           #' @seealso [plot()] para generacion en linea
                           #' @param data  Definicion del diagrama o fichero con la misma
                           #' @param caption Titulo de la imagen
                           #' @return la cadena del link
                           ,link              = function(data = NULL, caption = NULL) {
                               imgFile = private$makeImage(data, type)
                               target  = file.path(self$getOutputDir(), basename(imgFile))
                               paste0('[![', caption, "](", target, " \'", caption, "\')](https://127.0.0.1)")
                           }
                           #' @description Almacena el fichero de la imagen en el sistema de archivos
                           #' @seealso [plot()] para generacion en linea
                           #' @param data  Definicion del diagrama o fichero con la misma
                           #' @param caption Titulo de la imagen
                           #' @param force   Fuerza a regenerar el fichero
                           #' @return la cadena del link
                           ,file               = function(data=NULL, caption=NULL, force=NULL) {
                               private$makeImage(data, self$getType())
                           }
                           #' @description Carga un fichero de definicion de diagrama como clase S3PlantUML
                           #' @family generics
                           #' @param fileName  Path al fichero con la definicion
                           #' @return una clase S3PlantUML
                           ,load             = function(fileName=NULL) {
                               if (is.nul(fileName))        private$plantErr("E101", fileName)
                               if (!file.exists(fileName))  private$plantErr("E101", fileName)
                               tryCatch({
                                   data = readLines(fileName)
                                   structure(data, class = "S3PlantUML")
                               },error = function (e) {
                                   private$plantErr("E102", fileName)
                               }
                               )
                               names(data) = strsplit(fileName, ".", fixed = TRUE)[[1]]
                               private$removeUmlTags(data)
                           }

)
,private = list(nada=NULL
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
    ,parseR62          = function (object, full=FALSE, deep = FALSE) {
        priv = ""
        pub = ""
        uml = ""
        data = capture.output(object)
        data = trimws(data)
        inherit = grep("Inherits[ \t]*", data, ignore.case = TRUE, value = TRUE)
        if (length(inherit) > 0) {
            uml = private$parseR6Parent(inherit)
        }
        uml = c(uml, paste("class", class(object)[1], "{"))
        idxPublic  = grep("Public:" , data, ignore.case = TRUE)
        idxPrivate = grep("Private:", data, ignore.case = TRUE)
        if (idxPublic != 0) {
            max = ifelse(idxPrivate > 0, idxPrivate - 1, length(data))
            pub = private$extract(data[c((idxPublic + 1):max)],"+")
        }
        if (idxPrivate != 0 && full) {
            priv = "__ Private Data __"
            priv = c(priv, private$extract(data[c((idxPrivate + 1):length(data))],"-"))
        }
        uml = c(uml, pub, priv)
        structure(c(uml, "}"), class="S3PlantUML")
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
)
)

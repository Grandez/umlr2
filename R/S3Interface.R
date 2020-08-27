#' Funciones S3 pra interactuar con las clases R6
#  Mantenemos el sufijo 2 para que no se "pegue" con el paquete standalone
#' @title Interfaz S3
#' @description  La descripcion.

#' @export
umlr        = function(data=NULL, ...) { UMLR$new(data=data, ...)    }
#' @export
umlr.config = function(...) { CONFIG$new(...) }

############################################
# Generics
############################################
#' @export
plot       = function() UseMethod("plot")
#' @export
plot.UMLR2 = function() x$plot()

############################################
# Specifics
############################################

# rparse = function(...) {UseMethod("parse")}
#' @export
rparse = function(...) {
    parms = list(...)
    if (class(parms[[1]])[1] == "UMLR2") print("Pipe")
    print("Entra en parse")
}
#' @export
rclass = function(..., detail=NULL, deep=0) {
    parms = list(...)
    if (length(parms) > 0 && class(parms[[1]])[1] == "R6UMLR") {
        stacks = sys.calls()
        invocation = as.character(stacks[length(stacks)])
        invocation = gsub("rclass(.,", "$addClass(", invocation, fixed=TRUE)
        eval(parse(text=paste0("parms[[1]]", invocation)))
    }
    else {
       sys.calls()[[1]]
    }
}

#' @export
rpackage = function(name=name, ..., style=NULL) {
    parms = list(...)
    if (length(parms) > 0 && class(parms[[1]])[1] == "R6UMLR") {
        stacks = sys.calls()
        invocation = as.character(stacks[length(stacks)])
        invocation = gsub("rclass(.,", "$addPackage(", invocation, fixed=TRUE)
        eval(parse(text=paste0("parms[[1]]", invocation)))
    }
    else {
       sys.calls()[[1]]
    }
}

#' @export
rnamespace = function(name=name, ..., style=NULL) {
    parms = list(...)
    if (length(parms) > 0 && class(parms[[1]])[1] == "R6UMLR") {
        stacks = sys.calls()
        invocation = as.character(stacks[length(stacks)])
        invocation = gsub("rclass(.,", "$addNamespace(", invocation, fixed=TRUE)
        eval(parse(text=paste0("parms[[1]]", invocation)))
    }
    else {
       sys.calls()[[1]]
    }
}

#' @export
rstyle = function(style) {
    parms = list(...)
    if (length(parms) > 0 && class(parms[[1]])[1] == "R6UMLR") {
        stacks = sys.calls()
        invocation = as.character(stacks[length(stacks)])
        invocation = gsub("rclass(.,", "$addNamespace(", invocation, fixed=TRUE)
        eval(parse(text=paste0("parms[[1]]", invocation)))
    }
    else {
       sys.calls()[[1]]
    }
}

#' @export
rheader = function(style) {
}

#' @export
rfooter = function(style) {
}

#' @export
rinclude = function(style) {
}

#' @export
rblock = function(style) {
}


#' @export
rconfig = function(...) { CONFIG$new(...) }

# La sobrecarga definida funciona con:
#   - UMLR2 + funcion
#   - funcion + UMLR2
# Asi que es necesario verificar quien es el objeto principal
#' @export
"+.UMLR2" <- function(e1, e2) { # ee=expression(e2)) {
    browser()
    if ("UMLR2" %in% class(e1)) {
        obj = e1
        method = e2
    }
    else {
        obj = e2
        method = e1
    }
    browser()
    func = capture.output(method)
    nFunc = paste0("add", toupper(substr(func,2,2)), substring(func, 3))

    eval(base::parse(text=paste0(quote(obj), "$", nFunc)))
    obj
}

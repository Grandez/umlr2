#' La clase para gestionar el lenguaje de PlantUML
#' @title R6PlantSyntax
#' @docType class
#' @description  La clase para gestionar el lenguaje de PlantUML
PlantSyntax = R6::R6Class("R6PlantSyntax", inherit = PLANTUML,
    public = list(
         method  = function(name, private) paste(ifelse(private, "-", "+"), "{method}", name)
        ,field   = function(name, private) paste(ifelse(private, "-", "+"), "{field}",  name)
        ,binding = function(name, private) paste0("# ", "{field} \\",  name, "\\")
        ,inherit = function(cls, parent)   paste(parent, "<|--", cls)
        ,use     = function(cls, used)     paste(cls,  "o--",  used)
        ,comp    = function(cls, inc)      paste(cls,  "*--",  used)
        ,rel     = function(cls1, cls2)    paste(cls1, "--" ,  cls2)
        ,dot     = function(cls1, cls2)    paste(cls1, ".." ,  cls2)
        ,object  = function(name, generator=NULL, type=NULL) PlantClass$new(name, generator,type)
    )
)

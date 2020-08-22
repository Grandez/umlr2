# #' Genera una instancia unica de la clase de mensajes
# #' @export
# UMLR2Msg.getInstance = function() {
#     if (!exists("UMLRMsg")) {
#         cls = UMLR2MSG$new()
#         env = globalenv()
#         env$UMLR2Msg = cls
#     }
#     env$UMLR2Msg
# }
#' Clase con los mensajes de error
#' @title UMLRMSG
#' @docType class
#' @description  Contiene los mensajes de error
UMLR2MSG = R6::R6Class("R6UMLR2MSG"
    ,portable     = FALSE
    ,lock_objects = TRUE
    ,lock_class   = TRUE
    ,public       = list(
       #' @description Consructor protegido
       #' @details Esta clase no puede ser instanciada
       #'          Es un sigleton gestionado por UMLRMsg.getInstance
       #' @return La instancia del objeto
       initialize         = function() {
           # call = as.character(sys.call(-2))
           # if (call != "UMLR2Msg.getInstance") self$plantErr("E900", "UMLR2SG")
       }
       #' @description Genera un mensaje desde la tabla de mensajes
       #' @param code Codigo de error
       #' @param newCode Si existe reemplaza al anterior
       #' @param ... Informacion necesaria para el mensaje concreto
       ,msg = function(code, ..., newCode=0) {
            private$mountMessage(code, ..., newCode)
       }
       #' @description Genera un aviso
       #' @param code Codigo de aviso
       #' @param newCode Si existe reemplaza al anterior
       #' @param ... Informacion necesaria para el mensaje concreto
       ,warning = function(code, ..., newCode=0) {
          warning(private$mountMessage(code, ..., newCode))
       }
       #' @description Genera un mensaje de error y lanza la excepcion
       #' @param code Codigo de error
       #' @param data Datos del error
       #' @param newCode Si existe reemplaza al anterior
       #' @param ... Informacion necesaria para el mensaje concreto
       ,err = function(code, ..., data=NULL, newCode=0) {
         text = private$mountMessage(code, ..., newCode)
         private$err2 = data
         stop(errorCondition(text, class=c("UMLR2Err", "error")))
       }
       #' @description Almacena informacion extendida de un error
       #' @param desc mensajes de error
       ,setExtErr = function (desc) {
           private$err2 = desc
           invisible(self)
       }
       #' @description Almacena informacion extendida de un error
       #' @param desc mensajes de error
       ,getExtErr = function () {
           private$err2
       }

       #' @description Devuelve OK KO en funcion del dato pasado
       #' @param value dato a evaluar
       #' @return OK/KO
       ,ok = function(value) {
         ifelse(as.logical(value), "OK", "KO")
       }
       #' @description Genera un mensaje de error y lanza la excepcion
       #' @param code Codigo de error
       #' @param newCode Si existe reemplaza al anterior
       #' @param ... Informacion necesaria para el mensaje concreto
       #' @return La instancia del objeto
       ,plantErr         = function(code, ..., newCode=0) {
            c <- errorCondition(text, class=c("UMLR2Err", "error"))
            stop(c)
        }
    )
  ,private = list(classError = "UMLR2Err"
      ,err2 = ""
      ,mountMessage = function(code, ..., newCode=0) {
       if (newCode != 0) {
         text = sprintf("UMLRE%03d - %s", newCode, private$msgErr[code])
       }
       else {
         msg = sprintf(private$msgErr[code], ...)
         text = sprintf("UMLR%s - %s", code, msg)
       }
       text
     }
     ,msgErr = c(
        R001="Invalid value for %s"
       ,R002="Invalid directory: %s"
       ,R003="Invalid parameter: %s"
       ,R005="Invalid flag: %s"
       ,R006="Invalid or missing value"
       ,R010="Invalid call. Parameter missing"
       ,R011="Data provided can not be casted to S3PlantUML class"
       ,R012="%s is read only. Do exists a setter?"
       ,R020="%s no es una instancia de clase"
       ,R101="JVM not found: %s"
       ,R102="Component not found: %s"
       ,R103="All values must be named"
       ,R104="Incorrect configuration names: %s"
       ,R105="All configuration parameters must be named"
       ,R106="COnfiguration parameters must be an string"
       ,R107="Invalid graphic format. Only png, jpg or svg are allowed"
       ,R310="Flags must be character"
       ,R311="Invalid flags: %s"
       ,R201="No info provided for diagram"
       ,R202="outputDir must be set"
       ,R203="force must be TRUE or FALSE"
       ,R204="This method requires a file"
       ,R205="Input file not found: %s"
       ,R206="Parameter %s must be set"
       ,I001="Checking JVM machine \t"
       ,I002="Checking Graphviz \t\t"
       ,I003="Checking plantuml.jar \t"
       ,I004="Checking execution \t\t"
       ,I005="Checking environment \t"
       ,I010="Checking configuration values:"
       ,I011="\tJVM\t\t\t"
       ,I012="\tPlantUML\t\t"
       ,I013="\tInput extension\t\t"
       ,I014="\tGraph type\t\t"
       ,I015="\tCharset\t\t\t"
       ,I016="\tInput directory\t\t"
       ,I017="\tOutput directory\t"
       ,E001="Error generating diagram"
       ,E101="Invalid file name: %s"
       ,E102="Reading file name: %s"
       ,E110="Unable access to temporal directory"
       ,E111="Unable access to config directories"
       ,E200="InputDir must be set to store diagrams"
    # Warnings
       ,W510="Style/Template \'%s\' does not exists"
    #
       ,E810="\'object\' is not an instance of S4 or R6 Classes"
       ,E900="class %s is abstract"
       ,E901="Method \'%s\' is an interface or it is virtual"
   )
  )
)

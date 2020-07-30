#' Mark the project as in local development
#'
#' Algunos tests no se pueden realizar en los sistemas de integracion continua
#' por que no tenemos la seguridad de tener el entorno correcto:
#'     JVM
#'     Graphviz
#'     Etc
#'
#' Este fichero debe tener la variable pkgEnv$local a FALSE en github
#' Para ello se recomienda subirlo la primera vez y ponerlo en .gitignore
#'
pkgEnv <- new.env()
pkgEnv$local <- TRUE

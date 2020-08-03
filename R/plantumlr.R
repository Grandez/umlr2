## #' A wrapper for PlantUML diagrams
## #'
## #' @docType class
## #' @export
## #' @format An \code{R6Class} generator object
## #'
## #' @section Methods:
## #' \describe{
## #'   \itemize{\code{initialize(...)}}{
## #'      Crea un objeto PLANTUML
## #'      Acepta un conjunto de parametros nombre=valor_cadena donde nombre
## #'      identifica un parametro de configuracion
## #'   }
## #'   \itemize{\code{plot(data[, type])}{
## #'      Dibuja el diagrama
## #'      data es la definicion del diagrama via fichero o en linea
## #'      type define el formato del grafico, por defecto "png"
## #'   }
## #'   \itemize{\code{file(data, caption[, force])}{
## #'      Devuelve la referencia a la imagen almacenada en un fichero
## #'      data es la definicion del diagrama via fichero
## #'      caption El mensaje a poner en el fichero
## #'      force   ver caching
## #'   }
## #'   \itemize{\code{image(data)}{
## #'      Devuelve un objeto con la imagen generada
## #'      data es la definicion del diagrama via fichero o en linea
## #'   }
## #'   \itemize{\code{checkInstallation([verbose, first])}{
## #'      Verifica si la instalacion de los componentes es correcta
## #'      verbose Muestra mensajes de progreso
## #'      first   Si establecido, el proceso se detiene en el primer error
## #'   }
## #' }
NULL
#' plantumlr: Una herramienta para integrar diagramas generados con PlantUML
#'
#' @docType package
#' @name plantumlr
#' @description  Aqui va la descripcion
#' @section Disclaimer:
#' The documentation in this package is written intentionally in Spanish
#'
#' But if you feel uncomfortable with this language and prefer English,
#' do not hesitate to email me and I will translate it for you
#'
#' Sorry for the inconvenience
#'
#' @section Motivacion:
#' Segun vas utilizando las facilidades de RMarkdown para escribir documentos
#' mas temprano que tarde surge la necesidad de incluir en estos documentos imagenes y
#' graficos que no pueden ser generados directamente desde RMarkdown.
#'
#' En este paquete hablamos principalmente de la inclusion de diagramas UML.
#' Una herramienta popular para la generación de estos diagramas es **PlantUML**, un
#' proyecto Open Source que permite crear diagramas a partir de una definición de texto
#' "_usando un lenguaje simple e intuitivo_" (sic)
#'
#' Si bien es relativamente sencillo incluir el codigo especifico en cada documento, parece
#' mas adecuado crear un paquete que facilite este proceso, evite la repetición del código
#' en multiples documentos y aporte un flujo de trabajo normalizado para este tipo de diagramas.
#'
#' @section Requerimientos:
#' Este paquete integra en la documentación generada con RMarkdown la funcionalidad del
#' proyecto PlantUML, por lo que necesita que los siguientes componentes esten instalados
#' localmente:
#'
#' - Java
#' - Graphviz
#' - Paquete plantuml.jar
#'
#' La informacion detallada para su instalacion esta disponible en
#' \href{https://plantuml.com/es/faq-install}{Instalando plantuml}
#'
#' For more details see the help vignette:
#' \code{vignette("PlantUML in R", package = "plantumlr")}
NULL

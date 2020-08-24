options( pkgName="UMLR2"
        ,umlr2.repo = new.env())


umlr2.env          <- new.env()
umlr2.env$UMLR2Msg <- UMLR2MSG$new()

#' env.UMLR2 <- new.env()
#'
#' env.UMLR2$files =  Files$new()
#'
#' #' Genera una instancia unica de la clase de mensajes
#' env.UMLR2$UMLR2Msg.getInstance = function() {
#'     if (!exists("UMLRMsg")) {
#'         cls = UMLR2MSG$new()
#'         env = globalenv()
#'         env$UMLR2Msg = cls
#'     }
#'     env$UMLR2Msg
#' }

#' Genera una instancia unica de la clase de mensajes
# UMLR2Msg.getInstance = function() {
#     if (!exists("UMLRMsg")) {
#         cls = UMLR2MSG$new()
#         env = globalenv()
#         env$UMLR2Msg = cls
#     }
#     env$UMLR2Msg
# }

# To set custom options for your package with options().
# To avoid conflicts with other packages, ensure that you prefix option names with the name of your package.
# Also be careful not to override options that the user has already set.

# I use the following code in devtools to set up useful options:
.onLoad <- function(libname, pkgname) {
     print("Carga la libreria")
#     op <- options()
#     op.devtools <- list(
#         # devtools.path = "~/R-dev",
#         # devtools.install.args = "",
#         devtools.name = "Your name goes here",
#         devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
#         devtools.desc.license = "What license is it under?",
#         devtools.desc.suggests = NULL,
#         devtools.desc = list()
#     )
#     toset <- !(names(op.devtools) %in% names(op))
#     if(any(toset)) options(op.devtools[toset])
#
 invisible()
}
#
# .onLoad <- function(libpath) {
#     invisible()
# }

# registerType <- function(name, as, inst = NULL, singleton = FALSE,
#                          overwrite = FALSE, strict = 3, repo = getOption("solidr.repo")) {
#     gen <- get(name)
#     ## TODO: inherits = TRUE/FALSE? Some other restrictions necessary
#     ## (e.g. `as.environment("package:xyz")`)?
#
#     if (!exists(as, envir = repo, inherits = FALSE) || overwrite) {
#         if (!singleton) {
#             assign(as, gen, repo)
#         } else {
#
#             if (is.null(inst)) {
#                 attributes(gen)$singleton <- TRUE
#                 assign(as, gen, repo)
#             } else {
#                 attributes(inst)$singleton <- TRUE
#                 assign(as, inst, repo)
#             }
#         }
#     } else {
#         if (strict == 3) {
#             stop(sprintf("Already registered: %s as %s",
#                          get(as, repo)$classname, as))
#         }
#     }
# }
#
# handleOndemandSingletonRegistration <- function(
#     name,
#     gen,
#     inst,
#     repo = getOption("solidr.repo")
# ) {
#     if (!is.null(attributes(gen)$singleton)) {
#         registerType(name = gen$classname, as = name, inst = inst,
#                      singleton = TRUE, overwrite = TRUE, repo = repo)
#     }
# }

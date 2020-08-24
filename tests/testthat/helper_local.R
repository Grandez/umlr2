localTest <<- TRUE

checkLocal <- function() {
  if (nchar(Sys.getenv("PLANTUMLR_DEV")) == 0) skip("Test available only on local environment")
  #if (!exists("localTest")) skip("Test available only on local environment")
}

localDevelopment <- function() {
  (nchar(Sys.getenv("PLANTUMLR_DEV")) > 0)
}

getPlantUML  <- function() { "C:\\SDK\\plantuml\\plantuml.jar" }
getTestData  <- function() { "@startuml \n   [*]-->STATE1\n@enduml" }
getTestData1 <- function() { "@startuml \n   [*]-->STATE1\n@enduml" }

CLASS00 = R6::R6Class("R6CLASS00",
                      public = list(
                          initialize = function(...) { }
                          ,method00  = function()    { }
                      )
                      ,private = list (
                          .var00    = NA
                          ,.method00 = function(data) {}
                      )
)
CLASS10 = R6::R6Class("R6CLASS10", inherit=CLASS00,
                      public = list(
                          attr101    = NULL
                          ,attr102    = NULL
                          ,initialize = function() { CLASS20$new() }
                          ,finalize   = function() {}
                          ,method101  = function() {}
                      )
                      ,private = list(
                          .priv101   = 1
                          ,.method101 = function(dato) {CLASS21$new() }
                      )
)
CLASS20 = R6::R6Class("R6CLASS20",
                      public = list(
                          attr201    = NULL
                          ,attr202    = NULL
                          ,initialize = function() {}
                          ,finalize   = function() {}
                          ,method201  = function() {}
                      )
                      ,private = list(
                          .priv201   = 1
                          ,.method201 = function(dato) {}
                      )
)
CLASS21 = R6::R6Class("R6CLASS21",
                      public = list(
                          attr211    = NULL
                          ,attr212    = NULL
                          ,initialize = function() {}
                          ,finalize   = function() {}
                          ,method211  = function() {}
                      )
                      ,private = list(
                          .priv211   = 1
                          ,.method211 = function(dato) {}
                      )
)

c00 = CLASS00$new()
c10 = CLASS10$new()
c20 = CLASS20$new()
c21 = CLASS21$new()

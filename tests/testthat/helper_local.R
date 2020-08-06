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

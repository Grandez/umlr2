context("Configuration")

plant = PLANTUML$new()

checkLocal <- function() {
  if (nchar(Sys.getenv("PLANTUMLR_DEV")) == 0) skip("Test available only on local environment")
  #if (!exists("localTest")) skip("Test available only on local environment")
}

test_that("Extension", {
     plant$setExt(".txt")
     expect_false(plant$checkConfiguration(verbose=FALSE, first=TRUE))
     plant$setExt("uml")
})
test_that("Input Directory", {
     plant$setInputDir("pepe")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
     plant$setInputDir(NULL)
})
test_that("Input Directory", {
     plant$setOutputDir("pepe")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
     plant$setOutputDir(NULL)
})

test_that("checking JVM", {
     checkLocal()
     plant$setJVM("pepe")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking plantuml.jar (1)", {
     checkLocal()
     plant$setPlantUML("java")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking Graphviz", {
     checkLocal()
     oldDot = Sys.getenv("GRAPHVIZ_DOT")
     Sys.setenv(GRAPHVIZ_DOT = "nada")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
     Sys.setenv(GRAPHVIZ_DOT = oldDot)
})


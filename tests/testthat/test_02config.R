context("Configuration")

uml = UMLR$new()
cfg = uml$config

checkLocal <- function() {
  if (nchar(Sys.getenv("PLANTUMLR_DEV")) == 0) skip("Test available only on local environment")
  #if (!exists("localTest")) skip("Test available only on local environment")
}

test_that("Extension", {
     cfg$setExt(".txt")
     expect_false(cfg$checkConfiguration(verbose=FALSE, first=TRUE))
     cfg$setExt("uml")
})
test_that("Input Directory", {
     cfg$setInputDir("pepe")
     expect_false(cfg$checkInstallation(verbose=FALSE, first=TRUE))
     cfg$setInputDir(NULL)
})
test_that("Output Directory", {
     cfg$setOutputDir("pepe")
     expect_false(cfg$checkInstallation(verbose=FALSE, first=TRUE))
     cfg$setOutputDir(NULL)
})

test_that("checking JVM", {
     checkLocal()
     cfg$setJVM("pepe")
     expect_false(cfg$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking plantuml.jar (1)", {
     checkLocal()
     cfg$setPlantUML("java")
     expect_false(cfg$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking Graphviz", {
     checkLocal()
     oldDot = Sys.getenv("GRAPHVIZ_DOT")
     Sys.setenv(GRAPHVIZ_DOT = "nada")
     expect_false(cfg$checkInstallation(verbose=FALSE, first=TRUE))
     Sys.setenv(GRAPHVIZ_DOT = oldDot)
})


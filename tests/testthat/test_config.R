context("Configuration")

plant = PLANTUML$new()

checkLocal <- function() {
  #if (exists("localTest") && localTest) skip("Test available only on local environment")
}

test_that("checking JVM", {
     checkLocal()
     plant$setJVM("pepe")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking plantuml.jar (1)", {
     checkLocal()
     plant$setJVM("java")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
})
test_that("checking Graphviz", {
     checkLocal()
     oldDot = Sys.getenv("GRAPHVIZ_DOT")
     Sys.setenv(GRAPHVIZ_DOT = "nada")
     expect_false(plant$checkInstallation(verbose=FALSE, first=TRUE))
     Sys.setenv(GRAPHVIZ_DOT = oldDot)
})

test_that("checking plantuml.jar (2)", {
     checkLocal()
     plant$setPlantUML("E:\\SDK\\plantuml\\plantuml.jar")
     expect_true(plant$checkInstallation(verbose=FALSE, first=TRUE))
})

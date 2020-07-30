context("checking configuration")

checkLocal <- function() {
  if (pkgEnv$local) skip("Test available only on local environment")
}
if (pkgEnv$local) {
  plant = PLANTUML$new()
  test_that("checking JVM") {
     checkLocal()
     plant.setJVM("pepe")
     expect_error(plant$checkInstallation(), class="plantUMLR")
  })
}

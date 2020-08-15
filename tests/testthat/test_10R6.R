context("Graphics")

if (localDevelopment()) {
   plant = UMLR$new(plantuml=getPlantUML())

   test_that("Image1", expect_true((nchar(getTestData()) > 0)))
   #test_that("Image", expect_true((class(plant$image(getTestData())) == "magick-image")))
}


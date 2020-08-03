context("Class generic")


test_that("checking class", {
    plant = PLANTUML$new()
    expect_true("PLANTUML" %in% class(plant))
})

context("Class generic")

test_that("Helper", expect_true(localTest))

test_that("checking class", {
    plant = PLANTUML$new()
    expect_true("R6PLANTUML" %in% class(plant))
})

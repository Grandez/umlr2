context("Class generic")

test_that("Helper", expect_true(localTest))

test_that("checking class", {
    plant = UMLR$new()
    expect_true("R6UMLR" %in% class(plant))
})

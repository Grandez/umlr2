context("checking class creation")


test_that("checking class", {
    expect_true(is.element("PLANTUML", plant = PLANTUML$new()))
})

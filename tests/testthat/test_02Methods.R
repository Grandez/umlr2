context("Metodos")

umlr = UMLR$new()

# addClass

test_that("Invalid call to addClass (0)",   expect_error(ruml$addClass(),               class = "UMLRErr"))
test_that("Invalid call to addClass (1)",   expect_error(ruml$addClass(c10,c10),        class = "UMLRErr"))
test_that("Invalid call to addClass (2)",   expect_error(ruml$addClass(c10,c10,c10),    class = "UMLRErr"))
test_that("Invalid call to addClass (3)",   expect_error(ruml$addClass(c10,detail="x"), class = "UMLRErr"))
test_that("Invalid call to addClass (4)",   expect_error(ruml$addClass(c10,deep="x"),   class = "UMLRErr"))

test_that("Invalid call to addClass (5)",   expect_error(ruml$addClass("text"),         class = "UMLRErr"))
test_that("Invalid call to addClass (6)",   expect_error(ruml$addClass(33),             class = "UMLRErr"))

# addPackage

test_that("Invalid call to addPackage (0)",   expect_error(ruml$addPackage(),                  class = "UMLRErr"))
test_that("Invalid call to addPackage (1)",   expect_error(ruml$addPackage(c10),               class = "UMLRErr"))
test_that("Invalid call to addPackage (2)",   expect_error(ruml$addPackage("pkg01",style="x"), class = "UMLRErr"))

# test_that("Helper", expect_true(localTest))
#
# test_that("checking class", {
#     plant = UMLR$new()
#     expect_true("R6UMLR" %in% class(plant))
# })

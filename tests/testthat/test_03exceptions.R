context("Exceptions")

umlr = UMLR$new()
cfg = umlr$config

# Configuracion

# Empty values
test_that("Invalid setJVM",      expect_error(cfg$setJVM(),      class = "UMLR2Err"))
test_that("Invalid setPlantUML", expect_error(cfg$setPlantUML(), class = "UMLR2Err"))
test_that("Invalid SetExt",      expect_error(cfg$setExt(),      class = "UMLR2Err"))
test_that("Invalid setType",     expect_error(cfg$setType(),     class = "UMLR2Err"))
test_that("Invalid setCharset",  expect_error(cfg$setCharset(),  class = "UMLR2Err"))

# Numeric values
test_that("Invalid setJVM",      expect_error(cfg$setJVM(1L),      class = "UMLR2Err"))
test_that("Invalid setPlantUML", expect_error(cfg$setPlantUML(1L), class = "UMLR2Err"))
test_that("Invalid setInputDir", expect_error(cfg$setInputDir(1L), class = "UMLR2Err"))
test_that("Invalid SetExt",      expect_error(cfg$setExt(1L),      class = "UMLR2Err"))
test_that("Invalid setType",     expect_error(cfg$setType(1L),     class = "UMLR2Err"))
test_that("Invalid setCharset",  expect_error(cfg$setCharset(1L),  class = "UMLR2Err"))

# Multiple values
test_that("Invalid setJVM",      expect_error(cfg$setJVM(c("A", "B")),      class = "UMLR2Err"))
test_that("Invalid setPlantUML", expect_error(cfg$setPlantUML(c("A", "B")), class = "UMLR2Err"))
test_that("Invalid setInputDir", expect_error(cfg$setInputDir(c("A", "B")), class = "UMLR2Err"))
test_that("Invalid SetExt",      expect_error(cfg$setExt(c("A", "B")),      class = "UMLR2Err"))
test_that("Invalid setType",     expect_error(cfg$setType(c("A", "B")),     class = "UMLR2Err"))
test_that("Invalid setCharset",  expect_error(cfg$setCharset(c("A", "B")),  class = "UMLR2Err"))

# Invalid String
test_that("Invalid setJVM",      expect_error(cfg$setJVM(""),      class = "UMLR2Err"))
test_that("Invalid setPlantUML", expect_error(cfg$setPlantUML(""), class = "UMLR2Err"))
test_that("Invalid setInputDir", expect_error(cfg$setInputDir(""), class = "UMLR2Err"))
test_that("Invalid SetExt",      expect_error(cfg$setExt(""),      class = "UMLR2Err"))
test_that("Invalid setType",     expect_error(cfg$setType(""),     class = "UMLR2Err"))
test_that("Invalid setCharset",  expect_error(cfg$setCharset(""),  class = "UMLR2Err"))

test_that("Invalid setJVM",      expect_error(cfg$setJVM("  "),      class = "UMLR2Err"))
test_that("Invalid setPlantUML", expect_error(cfg$setPlantUML("  "), class = "UMLR2Err"))
test_that("Invalid setInputDir", expect_error(cfg$setInputDir("  "), class = "UMLR2Err"))
test_that("Invalid SetExt",      expect_error(cfg$setExt("  "),      class = "UMLR2Err"))
test_that("Invalid setType",     expect_error(cfg$setType("  "),     class = "UMLR2Err"))
test_that("Invalid setCharset",  expect_error(cfg$setCharset("  "),  class = "UMLR2Err"))

# Invalid config
test_that("Invalid config flag",  expect_error(cfg$setConfig(pepe="Pepe"),  class = "UMLR2Err"))
test_that("Invalid config value", expect_error(cfg$setConfig(jvm=1),        class = "UMLR2Err"))

# Invalid Type
test_that("Invalid Type value", expect_error(cfg$setConfig(type="xxx"),     class = "UMLR2Err"))
test_that("Invalid Type value", expect_error(cfg$setType("xxx"),            class = "UMLR2Err"))


context("Exceptions")

plant = PLANTUML$new()

# Configuracion

# Empty values
test_that("Invalid setJVM",      expect_error(plant$setJVM(),      class = "plantUMLErr"))
test_that("Invalid setPlantUML", expect_error(plant$setPlantUML(), class = "plantUMLErr"))
test_that("Invalid SetExt",      expect_error(plant$setExt(),      class = "plantUMLErr"))
test_that("Invalid setType",     expect_error(plant$setType(),     class = "plantUMLErr"))
test_that("Invalid setCharset",  expect_error(plant$setCharset(),  class = "plantUMLErr"))

# Numeric values
test_that("Invalid setJVM",      expect_error(plant$setJVM(1L),      class = "plantUMLErr"))
test_that("Invalid setPlantUML", expect_error(plant$setPlantUML(1L), class = "plantUMLErr"))
test_that("Invalid setInputDir", expect_error(plant$setInputDir(1L), class = "plantUMLErr"))
test_that("Invalid SetExt",      expect_error(plant$setExt(1L),      class = "plantUMLErr"))
test_that("Invalid setType",     expect_error(plant$setType(1L),     class = "plantUMLErr"))
test_that("Invalid setCharset",  expect_error(plant$setCharset(1L),  class = "plantUMLErr"))

# Multiple values
test_that("Invalid setJVM",      expect_error(plant$setJVM(c("A", "B")),      class = "plantUMLErr"))
test_that("Invalid setPlantUML", expect_error(plant$setPlantUML(c("A", "B")), class = "plantUMLErr"))
test_that("Invalid setInputDir", expect_error(plant$setInputDir(c("A", "B")), class = "plantUMLErr"))
test_that("Invalid SetExt",      expect_error(plant$setExt(c("A", "B")),      class = "plantUMLErr"))
test_that("Invalid setType",     expect_error(plant$setType(c("A", "B")),     class = "plantUMLErr"))
test_that("Invalid setCharset",  expect_error(plant$setCharset(c("A", "B")),  class = "plantUMLErr"))

# Invalid String
test_that("Invalid setJVM",      expect_error(plant$setJVM(""),      class = "plantUMLErr"))
test_that("Invalid setPlantUML", expect_error(plant$setPlantUML(""), class = "plantUMLErr"))
test_that("Invalid setInputDir", expect_error(plant$setInputDir(""), class = "plantUMLErr"))
test_that("Invalid SetExt",      expect_error(plant$setExt(""),      class = "plantUMLErr"))
test_that("Invalid setType",     expect_error(plant$setType(""),     class = "plantUMLErr"))
test_that("Invalid setCharset",  expect_error(plant$setCharset(""),  class = "plantUMLErr"))

test_that("Invalid setJVM",      expect_error(plant$setJVM("  "),      class = "plantUMLErr"))
test_that("Invalid setPlantUML", expect_error(plant$setPlantUML("  "), class = "plantUMLErr"))
test_that("Invalid setInputDir", expect_error(plant$setInputDir("  "), class = "plantUMLErr"))
test_that("Invalid SetExt",      expect_error(plant$setExt("  "),      class = "plantUMLErr"))
test_that("Invalid setType",     expect_error(plant$setType("  "),     class = "plantUMLErr"))
test_that("Invalid setCharset",  expect_error(plant$setCharset("  "),  class = "plantUMLErr"))

# Invalid config
test_that("Invalid config flag",  expect_error(plant$setConfig(pepe="Pepe"),  class = "plantUMLErr"))
test_that("Invalid config value", expect_error(plant$setConfig(jvm=1),        class = "plantUMLErr"))

# Invalid Type
test_that("Invalid Type value", expect_error(plant$setConfig(type="xxx"),     class = "plantUMLErr"))
test_that("Invalid Type value", expect_error(plant$setType("xxx"),            class = "plantUMLErr"))


# Ejecuciones
test_that("Invalid call",  expect_error(plant$plot(),  class = "plantUMLErr"))
test_that("Invalid call",  expect_error(plant$file(),  class = "plantUMLErr"))
test_that("Invalid call",  expect_error(plant$image(), class = "plantUMLErr"))

# Con file el directorio debe existir
test_that("Invalid Directory",  {
  plant$setOutputDir(NULL)
  expect_error(plant$file(), class = "plantUMLErr")
})

test_that("Invalid data", expect_error(plant$file("cosas uml"), class = "plantUMLErr"))
# TODO el fichero debe ser bueno
# test_that("Invalid force", expect_error(plant$file("fichero.uml", force=3), class = "plantUMLErr"))

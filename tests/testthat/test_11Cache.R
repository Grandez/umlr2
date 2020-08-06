context("Cache")

plant = PLANTUML$new(plantuml=getPlantUML())

testCreateFile = function(file=NULL) {
   fileIn = ifelse(is.null(file), paste0(tempfile(), ".uml"), file)
   writeLines(getTestData(), fileIn)
   fileIn
}
testFile = function(fileIn, fileOut) {
  plant$setOutputDir(dirname(fileOut))
  plant$file(fileIn, fileOut)
  test_that("File created", expect_true(file.exists(fileOut)))
}
testCacheOK = function(fileIn, fileOut) {
  tms = file.info(fileOut)$mtime
  testFile(fileIn, fileOut)
  test_that("Cache active", expect_identical(file.info(fileOut)$mtime, tms))
}
testCacheFail = function(fileIn, fileOut) {
  tms = file.info(fileOut)$ctime
  testFile(fileIn, fileOut)
  test_that("Cache fail", expect_gt(file.info(fileOut)$mtime, tms))
}
testCacheForce = function(fileIn, fileOut) {
  tms = file.info(fileOut)$ctime
  plant$setOutputDir(dirname(fileOut))
  plant$file(fileIn, fileOut, force=TRUE)
  test_that("Cache fail", expect_gt(file.info(fileOut)$mtime, tms))
}


if (localDevelopment()) {
    fileIn = testCreateFile()
    fileOut = gsub(".uml", ".png", fileIn, fixed=TRUE)
    if (file.exists(fileOut)) file.remove(fileOut)

    testFile(fileIn, fileOut)
    Sys.sleep(2)

    testCacheOK(fileIn, fileOut)
    Sys.sleep(2)

    fileIn = testCreateFile(fileIn)
    testCacheFail(fileIn, fileOut)
    Sys.sleep(2)

    testCacheForce(fileIn, fileOut)

    file.remove(fileIn)
    file.remove(fileOut)
}


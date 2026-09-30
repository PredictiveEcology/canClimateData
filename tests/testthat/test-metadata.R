## The module's metadata is its public contract: projects and downstream modules (e.g., the
## fireSense data-prep modules) bind to these object and parameter names. Renaming or retyping one
## breaks them, so a deliberate change updates this file in the same commit.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, "canClimateData")
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(inputs[order(names(inputs))],
                   c(climateVariables = "list", rasterToMatch = "SpatRaster", studyArea = "sf"))
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(outputs[order(names(outputs))],
                   c(historicalClimateRasters = "list", projectedClimateRasters = "list"))
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_setequal(
    md$parameters$paramName,
    c(".plotInitialTime", ".plotInterval", ".saveInitialTime", ".saveInterval", ".studyAreaName",
      ".useCache", "bufferDist", "climateGCM", "climateSSP", "historicalClimatePeriod",
      "historicalClimateYears", "outputDir", "projectedClimatePeriod", "projectedClimateYears",
      "projectedType", "quickCheck")
  )
})

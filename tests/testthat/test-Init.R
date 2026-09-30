## Init(): argument checks, and the names of the objects it hands downstream. The fireSense
## data-prep modules look climate layers up as `year<YYYY>`, so those names are part of the contract.

test_that("Init() rejects an unknown projectedType", {
  withr::local_options(reproducible.useCache = FALSE, reproducible.useNewDigestAlgorithm = 2)
  expect_error(runInit(path = withr::local_tempdir(), projectedType = "nowcast"), "projectedType")
})

test_that("Init() rejects a climateGCM or climateSSP that is not available", {
  withr::local_options(reproducible.useCache = FALSE, reproducible.useNewDigestAlgorithm = 2)
  path <- withr::local_tempdir()
  expect_error(runInit(path = path, params = list(climateGCM = "notAGCM")),
               "Invalid climate model specified")
  expect_error(runInit(path = path, params = list(climateSSP = 999)),
               "Invalid SSP scenario")
})

test_that("Init() splits the climate rasters by prefix and names layers by year or period", {
  withr::local_options(reproducible.useCache = FALSE, reproducible.useNewDigestAlgorithm = 2)
  sim <- runInit(path = withr::local_tempdir())

  expect_identical(names(sim$historicalClimateRasters), c("CMI_normal", "MDC"))
  expect_identical(names(sim$projectedClimateRasters), "MDC")
  expect_identical(names(sim$historicalClimateRasters$CMI_normal),
                   c("period1951_1980", "period1981_2010"))
  expect_identical(names(sim$historicalClimateRasters$MDC), c("year1991", "year1992"))
  expect_identical(names(sim$projectedClimateRasters$MDC), c("year2011", "year2012"))
  expect_s4_class(sim$historicalClimateRasters$MDC, "SpatRaster")
})

## The climate data are by calendar year (see climateData::checkCalendarYears(), which has the rule
## and its unit tests), so a run whose times are model years (setupProject() without `times` gives
## 0 to 10) cannot be served. This module must stop before preparing any climate data.

test_that("init stops, with a supplied climateVariables, on model-year times before touching any climate data", {
  rtm <- terra::rast(nrows = 2, ncols = 2, vals = 1L, crs = "EPSG:3978")
  sa <- sf::st_as_sf(terra::as.polygons(rtm))
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 0, end = 10), modules = moduleName, paths = testPaths,
    objects = list(studyArea = sa, rasterToMatch = rtm, climateVariables = list(dummy = list()))))
  expect_error(suppressMessages(SpaDES.core::spades(sim, debug = FALSE)),
               "canClimateData: times are model years 0-10.*calendar years.*climateVariables. was supplied")
})

test_that("init stops when the run starts before the climate years this module is asked for", {
  rtm <- terra::rast(nrows = 2, ncols = 2, vals = 1L, crs = "EPSG:3978")
  sa <- sf::st_as_sf(terra::as.polygons(rtm))
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 1950, end = 2020), modules = moduleName, paths = testPaths,
    params = list(canClimateData = list(historicalClimateYears = 1991:2022, projectedClimateYears = 2011:2100)),
    objects = list(studyArea = sa, rasterToMatch = rtm)))
  expect_error(suppressMessages(SpaDES.core::spades(sim, debug = FALSE)),
               "times are model years 1950-2020.*cover 1991-2100.*historicalClimateYears")
})

test_that("a rerun serves the simulation years its inputs load at (inputs(sim)), within times", {
  inp <- data.frame(objectName = c("a", "b", "a", "a", "a", "a"),
                    loadTime = c(2720, 2720, 2820, 2920, 3020, 3120))
  expect_identical(rerunSimYears(inp, 2720, 3020), c(2720, 2820, 2920, 3020))
  expect_identical(rerunSimYears(inp, 2720, 2720), 2720)
  ## no inputs: every year of times
  expect_equal(rerunSimYears(data.frame(loadTime = numeric(0)), 2720, 2722), c(2720, 2721, 2722))
})

test_that("a rerun prepares only the climate years the record holds for those simulation years", {
  latest <- climateData::latestHistoricalYear()
  rec <- data.table::data.table(simYear = 2719:2821,
                                climateYear = c(1990, 2001, rep(1995, 99), 1987, latest + 5))
  cv <- list(
    historical_CMD = list(vars = "historical_CMD", fun = quote(calcAsIs), .dots = list(historical_years = 1986:2024)),
    historical_CMI_normal = list(vars = "historical_CMI_normal", fun = quote(calcAsIs),
                                 .dots = list(historical_period = "1981_2010")),
    historical_cumMDC = list(vars = "historical_PPT01", fun = quote(calcCumMDC), .dots = list(historical_years = 1981:2024)),
    projected_MDC = list(vars = "future_PPT04", fun = quote(calcMDC), .dots = list(future_years = 2017:2100)))
  out <- climateVarsForRecord(cv, rec, c(2720, 2820, 2821))
  expect_identical(out$historical_CMD$.dots$historical_years, c(1987, 2001))
  expect_identical(out$projected_MDC$.dots$future_years, latest + 5)
  ## by period, and consecutive-year (calcCumMDC), left alone
  expect_identical(out$historical_CMI_normal, cv$historical_CMI_normal)
  expect_identical(out$historical_cumMDC, cv$historical_cumMDC)
  expect_error(climateVarsForRecord(cv, rec, c(2720, 2900)), "no climate year for simulation year\\(s\\) 2900")
})

test_that("with a climateYearRecord, init does not check times (model years) against calendar years", {
  rtm <- terra::rast(nrows = 2, ncols = 2, vals = 1L, crs = "EPSG:3978")
  sa <- sf::st_as_sf(terra::as.polygons(rtm))
  rec <- data.table::data.table(simYear = 2720, climateYear = 1987)
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 2720, end = 2720), modules = moduleName, paths = testPaths,
    objects = list(studyArea = sa, rasterToMatch = rtm, climateVariables = list(dummy = list()),
                   climateYearRecord = rec)))
  err <- tryCatch({suppressMessages(SpaDES.core::spades(sim, debug = FALSE)); ""},
                  error = function(e) conditionMessage(e))
  ## stops later, on the dummy climateVariables, not on the years
  expect_false(grepl("model years|climate year", err))
})

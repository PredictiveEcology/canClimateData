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

## The climate data are by calendar year (historical from 1901, projections to 2100), so a run
## whose times are model years (setupProject() without `times` gives 0 to 10) cannot be served.

test_that("calendar-year start times are accepted", {
  expect_silent(checkCalendarYears(2020, 2120))
  expect_silent(checkCalendarYears(1901, 1950))
  expect_silent(checkCalendarYears(2100, 2100))
  expect_silent(checkCalendarYears(1985.5, 2020))
})

test_that("model-year times fail with a message that says what to supply", {
  expect_error(checkCalendarYears(0, 10), "model years 0-10.*calendar years.*times = list\\(start = 2020")
  expect_error(checkCalendarYears(1900, 2000), "1901")
  expect_error(checkCalendarYears(2101, 2200), "2100")
})

test_that("init stops on model-year times before touching any climate data", {
  rtm <- terra::rast(nrows = 2, ncols = 2, vals = 1L, crs = "EPSG:3978")
  sa <- sf::st_as_sf(terra::as.polygons(rtm))
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 0, end = 10), modules = moduleName, paths = testPaths,
    objects = list(studyArea = sa, rasterToMatch = rtm, climateVariables = list(dummy = list()))))
  expect_error(suppressMessages(SpaDES.core::spades(sim, debug = FALSE)),
               "model years 0-10.*calendar years")
})

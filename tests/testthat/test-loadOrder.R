## This module reads `climateYearRecord` (from the climateYear module), and climateYear reads this
## module's climate rasters: a cycle, which SpaDES resolves with climateYear's init first.
## The `loadOrder` metadata keeps this module's init first, in either module order. The stub
## has climateYear's inputs, output and loadOrder; without this module's loadOrder it fails.

test_that("init is scheduled before climateYear's, in either module order", {
  mp <- withr::local_tempdir()
  file.symlink(moduleRoot, file.path(mp, moduleName))
  dir.create(file.path(mp, "climateYear"))
  writeLines(c(
    'defineModule(sim, list(name = "climateYear", description = "stub", keywords = "",',
    '  authors = person("A", "B"), childModules = character(0), version = list(climateYear = "0.0.1"),',
    '  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list(), documentation = list(),',
    '  loadOrder = list(before = "fireSense_dataPrepPredict"),',
    '  reqdPkgs = list(), parameters = rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, "stub")),',
    '  inputObjects = bindrows(expectsInput("historicalClimateRasters", "list", "stub"),',
    '    expectsInput("projectedClimateRasters", "list", "stub")),',
    '  outputObjects = createsOutput("climateYearRecord", "data.table", "stub")))',
    'doEvent.climateYear <- function(sim, eventTime, eventType) sim'),
    file.path(mp, "climateYear", "climateYear.R"))
  for (mods in list(c(moduleName, "climateYear"), c("climateYear", moduleName))) {
    sim <- suppressMessages(SpaDES.core::simInit(
      times = list(start = 2000, end = 2001), modules = mods,
      paths = modifyList(testPaths, list(modulePath = mp))))
    ev <- SpaDES.core::events(sim)
    inits <- ev$moduleName[ev$eventType == "init"]
    expect_identical(inits[inits %in% mods], c(moduleName, "climateYear"))
  }
})

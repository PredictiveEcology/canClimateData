## `outputDir` is part of the event cache keys, so an absolute path with a machine-specific prefix
## (the hindcast climate folder, <basePath>/outputs/hindcast<tag>/<ELF>/climate) made a cache built
## on one machine miss on another. `outputDir` may be `reproducible::asPath(dir, nParentDirs)`, which
## the key holds as only `dir`'s last `nParentDirs + 1` components. A different folder must still miss:
## a hit writes nothing into the new folder.

test_that("Init() uses a Path outputDir as the folder the climate files go to", {
  withr::local_options(reproducible.useCache = FALSE, reproducible.useNewDigestAlgorithm = 2)
  root <- withr::local_tempdir()
  dir <- file.path(root, "outputs", "hindcastX", "9.1", "climate")
  sim <- runInit(path = withr::local_tempdir(), params = list(outputDir = reproducible::asPath(dir, 2)))
  expect_true(all(startsWith(normalizePath(terra::sources(sim$historicalClimateRasters[[1]])),
                             normalizePath(dir))))
})

## a module with this module's parameter and a cached init, to read the key from the cache
writeKeyModule <- function(modulePath) {
  dir.create(file.path(modulePath, "m"), recursive = TRUE)
  cat(file = file.path(modulePath, "m", "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A", "B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("outputDir", "character", NA_character_, NA, NA, "d"),
    defineParameter(".useCache", "character", "init", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
')
}

test_that("a Path outputDir keys the event on its last components: same under another base, different for another folder", {
  skip_if_not_installed("SpaDES.core")
  withr::local_options(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE, spades.useRequire = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE)
  modulePath <- withr::local_tempdir(); cachePath <- withr::local_tempdir()
  writeKeyModule(modulePath)
  nKeys <- function(outputDir) {
    s <- suppressMessages(SpaDES.core::simInit(modules = "m", times = list(start = 0, end = 1),
                                               params = list(m = list(outputDir = outputDir)),
                                               paths = list(modulePath = modulePath, cachePath = cachePath)))
    suppressMessages(SpaDES.core::spades(s, debug = FALSE))
    length(unique(reproducible::showCache(cachePath, verbose = -2)$cacheId))
  }
  asP <- function(base, hindcast) reproducible::asPath(file.path(base, "outputs", hindcast, "9.1", "climate"), 2)
  expect_identical(nKeys(asP("/machineA", "hindcastX")), 1L)
  expect_identical(nKeys(asP("/other/machineB", "hindcastX")), 1L)   # same relative folder: a hit
  expect_identical(nKeys(asP("/machineA", "hindcastY")), 2L)         # another folder: a miss
  expect_identical(nKeys(file.path("/machineA", "outputs", "hindcastX", "9.1", "climate")), 3L)  # a plain path is keyed in full
})

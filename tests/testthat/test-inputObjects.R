## .inputObjects(): the `climateVariables` default, which needs no download. The `studyArea` and
## `rasterToMatch` defaults download data, so they are supplied here and not tested.

## Run the module's real .inputObjects() with stand-ins for the SpaDES accessors.
runInputObjects <- function(params = list(), objects = list()) {
  sim <- new.env()
  sim$params <- modifyList(list(projectedType = "forecast", .studyAreaName = "testSA",
                                historicalClimatePeriod = c("1951_1980", "1981_2010"),
                                historicalClimateYears = 2001:2003,
                                projectedClimateYears = 2031:2033), params)
  sim$studyArea <- "a supplied studyArea"
  sim$rasterToMatch <- "a supplied rasterToMatch"
  for (nm in names(objects)) sim[[nm]] <- objects[[nm]]

  mocks <- new.env(parent = environment(.inputObjects))
  mocks$P <- function(sim, ...) sim$params
  mocks[["P<-"]] <- function(sim, param, value) {
    sim$params[[param]] <- value
    sim
  }
  mocks$currentModule <- function(sim) "canClimateData"
  mocks$inputPath <- function(sim) tempdir()
  mocks$asPath <- function(obj, ...) obj
  mocks$suppliedElsewhere <- function(object, sim, ...) !is.null(sim[[object]])
  mocks$studyAreaName <- function(...) "hashedSA"
  mocks$mod <- new.env()

  inputObjects <- .inputObjects
  environment(inputObjects) <- mocks
  suppressMessages(inputObjects(sim))
}

monthly <- function(prefix) c(sprintf(paste0(prefix, "PPT%02d"), 4:9),
                              sprintf(paste0(prefix, "Tmax%02d"), 4:9))

test_that("forecast: historical CMI normals and MDC, and projected MDC from future climate", {
  cv <- runInputObjects()$climateVariables

  expect_identical(names(cv), c("historical_CMI_normal", "historical_MDC", "projected_MDC"))
  expect_identical(cv$historical_CMI_normal$vars, "historical_CMI_normal")
  expect_identical(cv$historical_CMI_normal$.dots, list(historical_period = c("1951_1980", "1981_2010")))
  expect_identical(cv$historical_MDC$vars, monthly("historical_"))
  expect_identical(cv$historical_MDC$.dots, list(historical_years = 2001:2003))
  expect_identical(cv$projected_MDC$vars, monthly("future_"))
  expect_identical(cv$projected_MDC$.dots, list(future_years = 2031:2033))
  expect_identical(cv$historical_MDC$fun, quote(calcMDC))
  expect_identical(cv$historical_CMI_normal$fun, quote(calcAsIs))
})

test_that("hindcast: projected CMI and MDC are prepared from historical climate", {
  cv <- runInputObjects(params = list(projectedType = "hindcast"))$climateVariables

  expect_identical(names(cv),
                   c("historical_CMI_normal", "historical_MDC", "projected_CMI", "projected_MDC"))
  expect_identical(cv$projected_CMI$vars, "historical_CMI")
  expect_identical(cv$projected_CMI$.dots, list(historical_years = 2001:2003))
  expect_identical(cv$projected_MDC$vars, monthly("historical_"))
  expect_identical(cv$projected_MDC$.dots, list(historical_years = 2001:2003))
})

test_that("a supplied climateVariables is kept", {
  mine <- list(historical_MDC = list(vars = "x", fun = quote(calcAsIs), .dots = list()))
  expect_identical(runInputObjects(objects = list(climateVariables = mine))$climateVariables, mine)
})

test_that("an NA .studyAreaName is replaced by a hash of studyArea", {
  sim <- runInputObjects(params = list(.studyAreaName = NA_character_))
  expect_identical(sim$params$.studyAreaName, "hashedSA")
})

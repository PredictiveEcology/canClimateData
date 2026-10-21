## Init() used `projected_yrs` for hindcasts, which is only defined inside .inputObjects(), so
## projectedType = "hindcast" failed with "object 'projected_yrs' not found".

test_that("sampleHindcastLayers() gives one layer per projected year, named by year", {
  hist <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, vals = 1:12,
                      names = paste0("MDC_historical_", 1991:1993))
  out <- sampleHindcastLayers(list(MDC = hist), years = 2011:2015)

  expect_identical(names(out), "MDC")
  expect_identical(names(out$MDC), paste0("year", 2011:2015))
  ## each output layer is one of the historical layers
  histVals <- lapply(seq_len(terra::nlyr(hist)), function(i) terra::values(hist[[i]], mat = FALSE))
  for (i in seq_len(terra::nlyr(out$MDC))) {
    v <- terra::values(out$MDC[[i]], mat = FALSE)
    expect_true(any(vapply(histVals, identical, logical(1), v)))
  }
})

test_that("sampleHindcastLayers() uses the same sampled layers for every climate variable", {
  mdc <- terra::rast(nrows = 2, ncols = 2, nlyrs = 5, vals = 1:20)
  cmi <- mdc * 10
  out <- withr::with_seed(1, sampleHindcastLayers(list(MDC = mdc, CMI = cmi), years = 2011:2030))

  expect_equal(terra::values(out$CMI), terra::values(out$MDC) * 10)
})

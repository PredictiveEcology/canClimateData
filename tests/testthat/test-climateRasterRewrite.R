## Regression tests for the climate-raster rewrite in Init(). The module has no simInit/Init test
## harness (tests/testthat only ever held the unmodified test-template.R), so these are unit-level
## tests around the extracted rewrite decision (`climateRastersToRewrite()`) and the rewrite itself
## (`writeUpdatedLayerNames()`), plus a direct check of the `Cache()` behaviour that makes the
## rewrite unnecessary when caching is on.

test_that("climateRastersToRewrite(): with caching on, historical is never rewritten; projected only under hindcast", {
  expect_identical(climateRastersToRewrite("forecast", useCache = TRUE), character(0))
  expect_identical(climateRastersToRewrite("hindcast", useCache = TRUE), "projected")
  expect_identical(climateRastersToRewrite("forecast", useCache = "overwrite"), character(0))
})

test_that("climateRastersToRewrite(): with caching off, both are rewritten", {
  both <- c("historical", "projected")
  expect_identical(climateRastersToRewrite("forecast", useCache = FALSE), both)
  expect_identical(climateRastersToRewrite("hindcast", useCache = FALSE), both)
  expect_identical(climateRastersToRewrite("forecast", useCache = 0), both)

  withr::local_options(reproducible.useCache = FALSE)
  expect_identical(climateRastersToRewrite("forecast"), both)
})

test_that("writeUpdatedLayerNames() writes the renamed layers to disk", {
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, nlyrs = 2, vals = 1:8), f, overwrite = TRUE)
  r <- terra::rast(f)
  terra::set.names(r, c("year2001", "year2002"))
  expect_false(identical(names(terra::rast(f)), names(r))) ## renaming alone leaves the file stale

  out <- writeUpdatedLayerNames(list(MDC = r))
  withr::defer(unlink(terra::sources(out$MDC)))

  expect_false(identical(terra::sources(out$MDC), f))
  expect_identical(names(out$MDC), c("year2001", "year2002"))
  expect_identical(names(terra::rast(terra::sources(out$MDC))), c("year2001", "year2002"))
  expect_equal(terra::values(out$MDC), terra::values(r))
})

test_that("writeUpdatedLayerNames() writes multi-layer stacks band-interleaved and tiled", {
  ## the input is pixel-interleaved (GDAL's default); the rewrite must not keep that layout
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 300, ncols = 300, nlyrs = 3, vals = seq_len(3 * 300^2)),
                     f, overwrite = TRUE)
  expect_true(any(grepl("INTERLEAVE=PIXEL", terra::describe(f))))
  f1 <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 300, ncols = 300, vals = seq_len(300^2)), f1, overwrite = TRUE)

  out <- writeUpdatedLayerNames(list(MDC = terra::rast(f), CMI = terra::rast(f1)))
  withr::defer(unlink(c(terra::sources(out$MDC), terra::sources(out$CMI))))

  info <- terra::describe(terra::sources(out$MDC))
  expect_true(any(grepl("INTERLEAVE=BAND", info)))
  expect_true(any(grepl("Block=256x256", info)))
  expect_equal(terra::values(out$MDC), terra::values(terra::rast(f)), tolerance = 0)
  ## a single layer is written with GDAL's defaults, as climateData writes it
  expect_equal(terra::values(out$CMI), terra::values(terra::rast(f1)), tolerance = 0)
})

test_that("Cache() restores a renamed file-backed raster's layer names without any rewrite", {
  tmpCache <- file.path(tempdir(), paste0("climateRasterRewriteCacheTest_", .Platform$OS.type))
  dir.create(tmpCache, showWarnings = FALSE)
  on.exit(unlink(tmpCache, recursive = TRUE), add = TRUE)

  makeRenamedFileBackedRaster <- function() {
    f <- tempfile(fileext = ".tif")
    r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)
    terra::writeRaster(r, f, overwrite = TRUE)
    out <- terra::rast(f)
    terra::set.names(out, "renamedLayer")
    out
  }

  r1 <- reproducible::Cache(makeRenamedFileBackedRaster, cachePath = tmpCache) # cache miss, writes tags
  r2 <- reproducible::Cache(makeRenamedFileBackedRaster, cachePath = tmpCache) # cache hit, restores tags

  expect_identical(terra::names(r1), "renamedLayer")
  expect_identical(terra::names(r2), "renamedLayer")
  expect_equal(terra::values(r2), terra::values(r1))
})

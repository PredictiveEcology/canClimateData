## Regression tests for the climate-raster rewrite defect (canClimateData.R Init(), ~line 232-240
## on `development`): every call to Init() rewrote `historicalClimateRasters` AND
## `projectedClimateRasters` to disk, purely to persist renamed layers, at ~2.3 GB per climate
## variable per study area. The module has no simInit/Init test harness (tests/testthat only ever
## held the unmodified test-template.R), so these are unit-level tests around the extracted
## rewrite decision (`climateRastersToRewrite()`), plus a direct check of the `Cache()` behaviour
## that makes the rewrite unnecessary in the forecast/historical case.

test_that("climateRastersToRewrite(): historical is never rewritten; projected only under hindcast", {
  expect_identical(climateRastersToRewrite("forecast"), character(0))
  expect_identical(climateRastersToRewrite("hindcast"), "projected")
  expect_false("historical" %in% climateRastersToRewrite("forecast"))
  expect_false("historical" %in% climateRastersToRewrite("hindcast"))
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

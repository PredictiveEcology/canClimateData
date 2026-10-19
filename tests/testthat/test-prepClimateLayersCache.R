## The Cache() call around climateData::prepClimateLayers() in Init() omitted names that are not
## arguments of that function, so the per-run srcdir/dstdir stayed in the key.

test_that("the omitted cache arguments are real arguments of prepClimateLayers()", {
  skip_if_not_installed("climateData")
  expect_true(all(prepClimateLayersOmitArgs() %in% names(formals(climateData::prepClimateLayers))))
})

test_that("different srcdir/dstdir give one cache entry", {
  cacheDir <- withr::local_tempdir("test_prepClimateLayersCache_")
  withr::local_options(reproducible.useMemoise = FALSE, reproducible.verbose = -2)
  fakePrep <- function(climateVarsList, srcdir, dstdir) climateVarsList
  for (run in c("rep1", "rep2")) {
    fakePrep(climateVarsList = "CMD", srcdir = file.path("in", run, "climate"),
             dstdir = file.path("out", run, "climate")) |>
      reproducible::Cache(omitArgs = prepClimateLayersOmitArgs(), cachePath = cacheDir)
  }
  expect_equal(length(unique(reproducible::showCache(cacheDir, verbose = -2)$cacheId)), 1L)
})

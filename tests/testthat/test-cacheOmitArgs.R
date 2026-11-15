## Regression test for canClimateData.R Init(): the `prepClimateLayers()` call is cached with
## `Cache(omitArgs = ...)`. `omitArgs` must name the *formal arguments* of the cached function;
## it named the module's local variables (`climatePath`, `climatePathOut`) instead, so nothing
## was omitted and the folder paths stayed in the cache key. Any change to the input or output
## folder (a new per-study-area folder, a different mount on another machine) then missed the
## cache and re-prepared the climate data.

## All `Cache(<fn>(...), ...)` calls in `expr` (the native pipe `<fn>(...) |> Cache(...)` parses
## to this form). Empty arguments (`x[, 1]`) are skipped: they cannot be passed around.
cacheCallsOf <- function(expr, fn) {
  if (!is.call(expr)) return(list())
  found <- if (identical(expr[[1]], quote(Cache)) && length(expr) > 1L && is.call(expr[[2]]) &&
               identical(expr[[2]][[1]], as.name(fn))) list(expr) else list()
  for (i in seq_along(expr)[-1L]) {
    if (!identical(expr[[i]], quote(expr = ))) found <- c(found, cacheCallsOf(expr[[i]], fn))
  }
  found
}

test_that("Init(): omitArgs of the cached prepClimateLayers() call are its formal arguments", {
  exprs <- parse(file.path(moduleRoot, paste0(moduleName, ".R")), keep.source = FALSE)
  calls <- unlist(lapply(exprs, cacheCallsOf, fn = "prepClimateLayers"), recursive = FALSE)
  expect_length(calls, 1L)

  ## default (no `outputDir`): per-run folder, so both folders stay out of the key
  omitArgs <- eval(calls[[1]]$omitArgs, list(sharedOut = FALSE))
  expect_true(all(c("srcdir", "dstdir") %in% omitArgs))
  expect_true(all(omitArgs %in% formalArgs(climateData::prepClimateLayers)),
              info = paste("not formals of prepClimateLayers():",
                           paste(setdiff(omitArgs, formalArgs(climateData::prepClimateLayers)),
                                 collapse = ", ")))
})

## With `outputDir` set, a cache entry made with another dstdir (the per-run default) must not
## be hit: reproducible restores its files under the run's outputPath, so the shared folder
## stayed unused and every run restored its own copy of the rasters.
test_that("Init(): dstdir is in the cache key when outputDir is set", {
  exprs <- parse(file.path(moduleRoot, paste0(moduleName, ".R")), keep.source = FALSE)
  calls <- unlist(lapply(exprs, cacheCallsOf, fn = "prepClimateLayers"), recursive = FALSE)
  omitArgs <- eval(calls[[1]]$omitArgs, list(sharedOut = TRUE))
  expect_false("dstdir" %in% omitArgs)
  expect_true("srcdir" %in% omitArgs)
})

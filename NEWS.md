# canClimateData (development version)

- Fixed: with `options(reproducible.useCache = FALSE)`, `Init()` again writes
  `historicalClimateRasters` and `projectedClimateRasters` to new files with the renamed layers.
  The previous change relied on `Cache()` to restore layer names, which does nothing when caching
  is off, so the files on disk kept the old names. Version 1.0.4.9002.
- Fixed: `Init()` (canClimateData.R) rewrote both `historicalClimateRasters` and
  `projectedClimateRasters` to disk after renaming their layers, even though
  `historicalClimateRasters` is never subset and the renamed, in-memory stack is already
  correct. `reproducible::Cache()` restores a file-backed SpatRaster's layer names from
  its cache tags, so the rewrite (and the second cached copy it produced) was unnecessary
  and wrote roughly 2.3 GB per climate variable per study area (about 9 GB per fireSense
  ELF). The rewrite is now kept only for `projectedClimateRasters` under hindcast, where
  `terra::subset()` samples layers with replacement and out of order, which `Cache()`
  cannot reliably restore on its own when the layer count is unchanged. Version 1.0.4.9001.

# canClimateData (development version)

- Fixed: `Init()` (canClimateData.R) rewrote both `historicalClimateRasters` and
  `projectedClimateRasters` to disk after renaming their layers, even though
  `historicalClimateRasters` is never subset and the renamed, in-memory stack is already
  correct. `reproducible::Cache()` restores a file-backed SpatRaster's layer names from
  its cache tags, so the rewrite (and the second cached copy it produced) was unnecessary
  and wrote roughly 2.3 GB per climate variable per study area (about 9 GB per fireSense
  ELF). The rewrite is now kept only for `projectedClimateRasters` under hindcast, where
  `terra::subset()` samples layers with replacement and out of order, which `Cache()`
  cannot reliably restore on its own when the layer count is unchanged. Version 1.0.4.9001.
- Fixed: `projectedType = "hindcast"` failed in `Init()` with "object 'projected_yrs' not found",
  because `projected_yrs` was only defined inside `.inputObjects()`. Hindcast layers are now
  sampled and named using `P(sim)$projectedClimateYears`.

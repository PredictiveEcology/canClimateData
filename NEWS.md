# canClimateData (development version)

- Floors raised to climateData >= 2.2.3.9008 (cells whose 12 monthly PPT values are all 0 become NA) and reproducible >= 3.2.1.9060 (`postProcessTo()` no longer forces `terraOptions(memfrac = 0)`, which smoothed every climate layer). The `init` event's cache key includes reqdPkgs, so cached climate layers built before these fixes are rebuilt once instead of being restored.

- Fixed: `Init()` (canClimateData.R) rewrote both `historicalClimateRasters` and
  `projectedClimateRasters` to disk after renaming their layers, even though
  `historicalClimateRasters` is never subset and the renamed, in-memory stack is already
  correct. `reproducible::Cache()` restores a file-backed SpatRaster's layer names from
  its cache tags, so the rewrite (and the second cached copy it produced) was unnecessary
  and wrote roughly 2.3 GB per climate variable per study area (about 9 GB per fireSense
  ELF). The rewrite is now kept only for `projectedClimateRasters` under hindcast, where
  `terra::subset()` samples layers with replacement and out of order, which `Cache()`
  cannot reliably restore on its own when the layer count is unchanged. Version 1.0.4.9001.

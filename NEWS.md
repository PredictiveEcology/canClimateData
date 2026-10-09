# canClimateData (development version)

- `projectedClimateYears` now defaults to 2017:2100 instead of 2011:2100. The projected archive for tile 39 (CNRM-ESM2-1 ssp370) has no 2013-2016 files, and observed climate now covers 1901-2024, so projections before 2017 are not needed.

# canClimateData 1.1.0

canClimateData now uses the current climateData package to prepare its climate layers. It can run hindcasts as well as forecasts, it returns historical and projected climate as named lists, and a shared output folder can be set for climate files. The manual was updated, with links for exploring and comparing climate scenarios.

The module no longer writes a second copy of every climate layer to disk. That copy took roughly 2.3 GB per climate variable per study area. Climate normals now have the right layer names, and hindcasts sample historical years correctly. The minimum versions of climateData and reproducible were raised to pick up fixes to monthly precipitation and to layer smoothing. Climate layers cached by earlier versions are rebuilt once.

# canClimateData 1.0.4.9002

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

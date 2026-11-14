# canClimateData (development version)

- Rerunning this module on a finished run whose years are not calendar years (NRV), with that run's `climateYearRecord` (from the climateYear module), now prepares the climate years the record holds for the simulation years the rerun serves (the load times of its inputs, `inputs(sim)`, which `alreadyRun` sets to the finished run's save times), not the years in `climateVariables` or `historicalClimateYears`/`projectedClimateYears`. Variables by period, and `calcCumMDC` ones (which need consecutive years), keep their years. `times` are not checked as calendar years then. `climateYearRecord` is a new optional input; in a normal run climateYear writes it after this module's `init`, and `loadOrder` keeps that order.
- `climateData` is now required from its modsForFireSense branch (>= 2.2.3.9011). `checkCalendarYears()` is only there; climateData development reached 2.2.3.9011 without it, so the old floor installed a climateData whose `init` failed with "could not find function".
- `projectedClimateYears` now defaults to 2017:2100 instead of 2011:2100. The projected archive for tile 39 (CNRM-ESM2-1 ssp370) has no 2013-2016 files, and observed climate now covers 1901-2024, so projections before 2017 are not needed.
- `init` now stops, before preparing any climate data, when the run's start is outside the climate years this module is asked for (`historicalClimateYears` and `projectedClimateYears`, 1991 to 2100 by default), for example the model years 0-10 that `setupProject()` gives without `times`. With a supplied `climateVariables`, whose years are not known here, it stops when the start is not a calendar year from 1901 to 2100. The check is `climateData::checkCalendarYears()` (climateData >= 2.2.3.9009); the message says how to proceed. The run may end after the last year.

# canClimateData 1.1.1

- Fixed: `projectedType = "hindcast"` failed in `Init()` with "object 'projected_yrs' not found",
  because `projected_yrs` was only defined inside `.inputObjects()`. Hindcast layers are now
  sampled and named using `P(sim)$projectedClimateYears`.

- Fixed: the `Cache()` of `prepClimateLayers()` in `Init()` (canClimateData.R) named the module's local variables (`climatePath`, `climatePathOut`) in `omitArgs` instead of the function's formal arguments (`srcdir`, `dstdir`), so nothing was omitted and the folder paths stayed in the cache key. Changing the input or output folder (a new per-study-area folder, a different mount on another machine) re-prepared the climate data. The cache key changes, so every study area re-prepares its climate data once. The result is file-backed under `dstdir`; `reproducible` restores it under the current `outputPath` (or the cache) on a hit, so omitting `dstdir` is safe.

- Fixed: with `options(reproducible.useCache = FALSE)`, `Init()` again writes
  `historicalClimateRasters` and `projectedClimateRasters` to new files with the renamed layers.
  1.1.0 relied on `Cache()` to restore layer names, which does nothing when caching
  is off, so the files on disk kept the old names. Multi-layer stacks are rewritten band-interleaved
  in 256 x 256 tiles, the layout climateData#33 proposes for `prepClimateLayers()`, so one year's
  layer can be read without decompressing the others.

# canClimateData 1.1.0

canClimateData now uses the current climateData package to prepare its climate layers. It can run hindcasts as well as forecasts, it returns historical and projected climate as named lists, and a shared output folder can be set for climate files. The manual was updated, with links for exploring and comparing climate scenarios.

The module no longer writes a second copy of every climate layer to disk. That copy took roughly 2.3 GB per climate variable per study area. Climate normals now have the right layer names, and hindcasts sample historical years correctly. The minimum versions of climateData and reproducible were raised to pick up fixes to monthly precipitation and to layer smoothing. Climate layers cached by earlier versions are rebuilt once.

- Floors raised to climateData >= 2.2.3.9008 (cells whose 12 monthly PPT values are all 0 become NA) and reproducible >= 3.2.1.9060 (`postProcessTo()` no longer forces `terraOptions(memfrac = 0)`, which smoothed every climate layer). The `init` event's cache key includes reqdPkgs, so cached climate layers built before these fixes are rebuilt once instead of being restored.

- Fixed: `Init()` (canClimateData.R) rewrote both `historicalClimateRasters` and
  `projectedClimateRasters` to disk after renaming their layers, even though
  `historicalClimateRasters` is never subset and the renamed, in-memory stack is already
  correct. `reproducible::Cache()` restores a file-backed SpatRaster's layer names from
  its cache tags, so the rewrite (and the second cached copy it produced) was unnecessary
  and wrote roughly 2.3 GB per climate variable per study area (about 9 GB per fireSense
  ELF). The rewrite is now kept only for `projectedClimateRasters` under hindcast, where
  `terra::subset()` samples layers with replacement and out of order, which `Cache()`
  cannot reliably restore on its own when the layer count is unchanged.

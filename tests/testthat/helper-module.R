## The module has no simInit() test harness, so load its functions directly: evaluate every
## top-level `name <- function(...)` in canClimateData.R, i.e., everything but defineModule().
for (e in parse(test_path("..", "..", "canClimateData.R"), keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
      identical(e[[3]][[1]], as.name("function"))) {
    eval(e)
  }
}
rm(e)

## a real run gets these from the module's reqdPkgs
Cache <- reproducible::Cache
.suffix <- reproducible::.suffix

## Run the module's real Init() on small file-backed rasters, with stand-ins for the SpaDES accessors
## and for climateData::prepClimateLayers(). The stand-in layer names end in the year or period, as
## climateData's do; Init() only uses that ending. `calls` counts calls to prepClimateLayers().
runInit <- function(path, projectedType = "forecast", calls = new.env()) {
  calls$n <- 0L
  sim <- new.env()
  sim$params <- list(projectedType = projectedType, outputDir = NA_character_,
                     .studyAreaName = "testSA", climateGCM = "CNRM-ESM2-1", climateSSP = 370,
                     projectedClimateYears = 2011:2012)
  sim$climateVariables <- list(historical_CMI_normal = list(), historical_MDC = list(),
                               projected_MDC = list())

  mocks <- new.env(parent = environment(Init))
  mocks$P <- function(sim, ...) sim$params
  mocks$inputPath <- mocks$outputPath <- function(sim) path
  mocks$checkPath <- function(path, create = FALSE) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path
  }
  mocks$asPath <- function(obj, ...) obj
  mocks$available <- function(...) list(gcms = "CNRM-ESM2-1", ssps = 370)
  mocks$.robustDigest <- function(...) NULL
  mocks$mod <- new.env()
  mocks$Par <- list(.useCloud = FALSE)
  mocks$prepClimateLayers <- function(climateVarsList, dstdir, ...) {
    calls$n <- calls$n + 1L
    fileBacked <- function(lyrNames) {
      f <- file.path(dstdir, paste0(lyrNames[1], ".tif"))
      terra::writeRaster(terra::rast(nrows = 2, ncols = 2, nlyrs = length(lyrNames),
                                     vals = seq_len(4 * length(lyrNames)), names = lyrNames),
                         f, overwrite = TRUE)
      terra::rast(f)
    }
    list(fileBacked(c("CMI_normal_historical_1951_1980", "CMI_normal_historical_1981_2010")),
         fileBacked(c("MDC_historical_1991", "MDC_historical_1992")),
         fileBacked(c("MDC_future_2011", "MDC_future_2012")))
  }

  init <- Init
  environment(init) <- mocks
  init(sim)
}

## layer names stored in the file behind a raster
fileLayerNames <- function(x) names(terra::rast(terra::sources(x)))

defineModule(sim, list(
  name = "canClimateData",
  description = paste(
    "Prepares projected and historical climate data for fitting and predicting fires,",
    "and calculating climate effects on forest growth and mortality."
  ),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "aut"),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "ctb")
  ),
  childModules = character(0),
  version = list(canClimateData = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "canClimateData.Rmd")),
  reqdPkgs = list("archive", "digest", "geodata", "googledrive", "purrr",
                  "R.utils", "sf", "spatialEco", "terra",
                  "PredictiveEcology/climateData@development (>= 2.0.0)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9046)",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9064)",
                  "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9018)", ## TODO: use development once merged
                  "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9007)", ## TODO: use development once merged
                  "PredictiveEcology/SpaDES.tools@development (>= 2.0.4.9002)"),
  parameters = rbind(
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer `studyArea` and `rasterToMatch`."),
    defineParameter("climateGCM", "character", "CNRM-ESM2-1", NA, NA,
                    paste("Global Circulation Model to use for climate projections:",
                          "currently 'CanESM5' or 'CNRM-ESM2-1'.")),
    defineParameter("climateSSP", "numeric", 370, NA, NA,
                    paste("SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.")),
    defineParameter("historicalFireYears", "numeric", default = 1991:2022, NA, NA,
                    paste("range of years captured by the historical climate data")),
    defineParameter("projectedFireYears", "numeric", default = 2011:2100, NA, NA,
                    paste("range of years captured by the projected climate data")),
    defineParameter("quickCheck", "logical", default = TRUE, NA, NA,
                    paste("`prepClimateData` uses `prepInputs` internally; set this to `TRUE` to avoid",
                          "the slow process of digesting potentially MANY files.",
                          "This will use `file.size` only, if `TRUE`.")),
    defineParameter("studyAreaName", "character", NA_character_, NA, NA,
                    paste("User-defined label for the current stuyd area.",
                          "If `NA`, a hash of `studyArea` will be used.")),
    defineParameter("usePrepInputs", "logical", TRUE, NA, NA,
                    "There are currently two ways to run this module: using `reproducible::prepInputs` and ",
                    "a custom approach using googledrive directly. The direct googledrive approach was the ",
                    "original approach; `usePrepInputs = TRUE` is a rewrite from that original code. On Dec 11, 2023, ",
                    "the two ways would be similar, but they may diverge over time."),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant."))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "template raster corresponding to `studyArea`.", sourceURL = NA),
    expectsInput("rasterToMatchReporting", "SpatRaster",
                 desc = "template raster corresponding to `studyAreaReporting`.", sourceURL = NA),
    expectsInput("studyArea", "sf",
                 desc = "study area used for simulation (buffered to mitigate edge effects)",
                 sourceURL = NA),
    expectsInput("studyAreaReporting", "sf",
                 desc = "study area used for reporting/post-processing", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ATAstack", "SpatRaster",
                  desc = "annual projected mean annual temperature anomalies"),
    createsOutput("CMIstack", "SpatRaster",
                  desc = "annual projected mean climate moisture deficit"),
    createsOutput("CMInormal", "SpatRaster",
                  desc = "Climate Moisture Index Normals from 1950-2010"),
    createsOutput("historicalClimateRasters", "list",
                  desc = "list of a single raster stack - historical MDC calculated from ClimateNA data"),
    createsOutput("projectedClimateRasters", "list",
                  desc = "list of a single raster stack - projected MDC calculated from ClimateNA data")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.canClimateData = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "canClimateData", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "canClimateData", "save")
    },
    warning(paste("Undefined event type: \"", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions ----------------------------------------------------------------------------

Init <- function(sim) {
  ## separate intermediate outputs from raw inputs, to reduce file conflicts on shared drives
  climatePath <- file.path(inputPath(sim), "climate") |> checkPath(create = TRUE) |> asPath(1)
  climatePathOut <- file.path(outputPath(sim), "climate") |> checkPath(create = TRUE) |> asPath(1)

  if (is.na(P(sim)$studyAreaName)) {
    ## use unique hash as study area name
    P(sim, "studyAreaName") <- studyAreaName(sim$studyArea)
  }

  ## ensure this matches mod$targetCRS defined in .inputObjects !!
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  availableGCMs <- available("future")[["gcms"]]
  if (!P(sim)$climateGCM %in% availableGCMs) {
    stop("Invalid climate model specified.\n",
         "climateGCM should be one of: ", paste(availableGCMs, collapse = ", "), ".")
  }

  availableSSPs <- available("future")[["ssps"]]
  if (!P(sim)$climateSSP %in% availableSSPs) {
    stop("Invalid SSP scenario for climate model ", P(sim)$climateGCM, "_ssp", P(sim)$climateSSP, ".\n",
         "climateSSP should be one of: ", paste(availableSSPs, collapse = ", "), ".")
  }

  digestSA_RTM <- .robustDigest(list(sim.studyArea = sim$studyArea,
                                     sim.rasterToMatch = sim$rasterToMatch))
  sim$studyArea$studyAreaName <- paste0(P(sim)$studyAreaName, collapse = "_") ## makes it a data.frame

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

  ## PREPARE CLIMATE LAYERS
  historic_prd <- c("1951_1980", "1981_2010")
  historic_yrs <- P(sim)$historicalFireYears
  future_yrs <- P(sim)$projectedFireYears

  GCM <- P(sim)$climateGCM
  SSP <- P(sim)$climateSSP

  climateVariables <- list(
    historic_CMI_normal = list(
      vars = "historic_CMI_normal",
      fun = quote(calcCMInormal),
      .dots = list(historic_period = historic_prd, historic_years = historic_yrs)
    ),
    historic_MDC = list(
      vars = c(sprintf("historic_PPT%02d", 4:9), sprintf("historic_Tmax%02d", 4:9)),
      fun = quote(calcMDC),
      .dots = list(historic_years = historic_yrs)
    ),
    future_ATA = list(
      vars = c("future_MAT", "historic_MAT_normal"),
      fun = quote(calcATA),
      .dots = list(historic_period = historic_prd, future_years = future_yrs)
    ),
    future_CMI = list(
      vars = "future_CMI",
      fun = quote(calcAsIs),
      .dots = list(future_years = future_yrs)
    ),
    future_MDC = list(
      vars = c(sprintf("future_PPT%02d", 4:9), sprintf("future_Tmax%02d", 4:9)),
      fun = quote(calcMDC),
      .dots = list(future_years = future_yrs)
    )
  )

  climateRasters <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    historic_years = historic_yrs,
    future_years = future_yrs,
    historic_period = historic_prd,
    future_period = NULL,
    gcm = GCM,
    ssp = SSP,
    cl = NULL, ## TODO: allow user to pass their own cl object to module
    studyArea = sim$studyArea,
    studyAreaName = P(sim)$studyAreaName,
    rasterToMatch = sim$rasterToMatch
  )

  ## rename list elements and layers to match expected names downstream
  names(climateRasters) <- names(climateVariables)

  names(climateRasters$historic_CMI_normal) |>
    gsub("CMI_normal_", "period", x = _) |>
    terra::set.names(climateRasters$historic_CMI_normal, value = _)

  names(climateRasters$historic_MDC) |>
    gsub("MDC_historic_", "year", x = _) |>
    terra::set.names(climateRasters$historic_MDC, value = _)

  names(climateRasters$future_ATA) |>
    gsub("ATA_future_", "year", x = _) |>
    terra::set.names(climateRasters$future_ATA, value = _)

  names(climateRasters$future_CMI) |>
    gsub("CMI_future_", "year", x = _) |>
    terra::set.names(climateRasters$future_CMI, value = _)

  names(climateRasters$future_MDC) |>
    gsub("MDC_future_", "year", x = _) |>
    terra::set.names(climateRasters$future_MDC, value = _)

  ## objects for LandR.CS:
  sim$ATAstack <- climateRasters$future_ATA
  sim$CMIstack <- climateRasters$future_CMI
  sim$CMInormal <- climateRasters$historic_CMI_normal

  ## objects for fireSense:
  sim$historicalClimateRasters <- climateRasters$historic_MDC
  sim$projectedClimateRasters <- climateRasters$future_MDC

  return(invisible(sim))

}
#' prepInputs for climate data
#'
#' @param studyAreaNamesShort A character vector of short names for the "regions", e.g., c("AB", "BC")
#' @param studyAreaNamesLong A character vector of long names for the "regions" e.g., c("Alberta", "British Columbia")
#' @param climateURLs A character vector of urls to pass -- one at a time -- to `prepInputs(url = ...)`
#' @param studyAreaName A character string that represents the studyArea for the project e.g., "Edehzhie"
#' @param climateYears A numeric vector of years to extract climate data for e.g., 1991:2020 or 2025:2100
#' @param rasterToMatch A `SpatRaster` to pass to `prepInputs` --> which will pass to `postProcessTo`.
#'   To skip `postProcessTo`, user must set this to `NULL` and `studyArea = NULL, saveInner = FALSE`.
#' @param studyArea A `SpatVector` to pass to `prepInputs` --> which will pass to `postProcessTo`.
#'   To skip `postProcessTo`, user must set this to `NULL` and `studyArea = NULL, saveInner = FALSE`.
#' @param currentModuleName A character string e.g., extracted from `currentModule(sim)`, defaults to `"NoModule"`
#' @param climatePath A character string that is passed to `prepInputs(..., destinationPath = climatePath)`.
#'   If `saveInner = TRUE` (the default), this will also be used to build the filename passed to
#'   `postProcessTo(..., writeTo = XXX)`.
#' @param digestSA_RTM A charater string that represents the a unique identifier of the `studyArea` and
#'   `rasterToMatch` objects, e.g., from `CacheDigest(list(studyArea, rasterToMatch))$outputHash`. This is
#'   used to save time internally so the many `prepInputs` calls don't have to keep digesting these two
#'   spatial objects.
#' @param climateType A character string that is used as a label for this call. Intended to be used
#'   to indicate e.g., "historical" or "projected" or more nuance, like "projected_annual_ATA".
#'
#' @param fun A quoted function call that is passed to `fun` argument in `prepInputs`.
#'   This can use `SANshort` (each of `studyAreaNamesShort`),
#'   `SANlong` (each of `studyAreaNamesLong`), `climatePath`, `climateURL` (each of `climateURL`),
#'   `climateYears`, or anything passed to `...`
#' @param climateVar A character string used as a label (for filenames and other labels for messaging)
#'   that indicates the type of climate variable
#'   being calculated/downloaded/derived e.g., `"MDC"` (which is the default).
#' @param saveInner Length 1 logical. Should `postProcessTo(...)` be passed a `writeTo` for each
#'   individual `studyAreaShort`
#' @param saveOuter Length 1 logical. After `mergeRaster` of all the `studyAreaShort`, should
#'   the final `SpatRaster` be saved to disk.
#' @param ... Any optional named argument needed for `fun`
#' @export
#'
prepClimateData <- function(studyAreaNamesShort,
                            studyAreaNamesLong, climateURLs, # these are vectorized on studyAreaNamesShort
                            studyAreaName, climateYears,
                            rasterToMatch = NULL, studyArea = NULL, currentModuleName = "NoModule",
                            climatePath, digestSA_RTM, climateType = c("historical", "projected"),
                            fun, climateVar = "MDC", saveInner = TRUE, saveOuter = TRUE, leadingArea, ...) {

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- inputPath(sim) |> asPath(1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  objsForDigest1 <- c("studyAreaName", "climateYears", "fun", "climatePath", "climateType", "currentModuleName")
  objs <- mget(objsForDigest1, envir = environment())
  dig1 <- .robustDigest(object = objs)
  climDatAll <-  Map(SANshort = studyAreaNamesShort,
                     SANlong = studyAreaNamesLong,
                     climateURL = climateURLs,
                     function(SANshort, SANlong, climateURL) {

                       cacheTags <- c(studyAreaName, currentModuleName)
                       # climateArchive <- file.path(climatePath, paste0(studyAreaNameDir[[SANshort]], ".zip"))
                       filenameForSaving <- NULL
                       if (isTRUE(saveInner))
                         filenameForSaving <- file.path(climatePath,
                                                        paste0(climateVar, "_", climateType[1], "_", SANshort, "_",
                                                               paste(studyAreaName, collapse = "_"), ".tif"))
                       climData <- Cache(
                         prepInputs(
                           # for preProcess
                           targetFile = NA_character_,
                           url = as_id(climateURL),
                           destinationPath = climatePath,
                           # archive = climateArchive,

  if (!suppliedElsewhere("studyArea", sim)) {
    ## random study area spanning portions of NE AB and NW SK
    sim$studyArea <- terra::vect(cbind(-110, 58), crs = "epsg:4326") |>
      SpaDES.tools::randomStudyArea(size = 5e10) |>
      sf::st_as_sf() |>
      sf::st_transform(mod$targetCRS) |>
      sf::st_buffer(P(sim)$bufferDist)
  }

                           # for postProcessTo
                           to = rasterToMatch,
                           maskTo = studyArea,
                           writeTo = filenameForSaving,
                           datatype = "INT2U",
                           useCache = FALSE, # This is the internal cache for postProcessTo

  if (is.na(P(sim)$studyAreaName)) {
    ## use unique hash as study area name
    P(sim, "studyAreaName") <- studyAreaName(sim$studyArea)
  }

  #temporary workaround (06/12/2023) to terra bug with NAflag when INT1U/INT2U passed
  if (climateVar == "MDC") {
    climDatAll <- lapply(climDatAll, `NAflag<-`, value = 0)
  }

  if (!suppliedElsewhere("rasterToMatchReporting", sim)) {
    sim$rasterToMatchReporting <- Cache(maskTo, sim$rasterToMatch, maskTo = sim$studyAreaReporting)
    writeRaster(sim$rasterToMatchReporting,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtmr.tif")),
                datatype = "INT1U", overwrite = TRUE)
  }
  message("Merging spatial layers using sf::gdal_utils('warp'...)")
  if (all(names(fns) %in% leadingArea)){
    fns <- rev(fns[leadingArea]) # Here we need to reverse, as the main area is the last one
  } else {
    fns <- c(fns[!names(fns) %in% leadingArea],
             rev(fns[leadingArea]))
  }
  system.time(sf::gdal_utils(util = "warp", source = fns,
                             destination = filenameForSaving,
                             options = "-overwrite"))
  climDatAllMerged <- terra::rast(filenameForSaving)
  names(climDatAllMerged) <- names(climDatAll[[1]])

  return(invisible(sim))
}

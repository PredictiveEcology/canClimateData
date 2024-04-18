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
  version = list(canClimateData = "1.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "canClimateData.Rmd")),
  reqdPkgs = list("archive", "digest", "geodata", "googledrive", "purrr",
                  "R.utils", "sf", "spatialEco", "terra",
                  "PredictiveEcology/climateData@development (>= 2.0.4)",
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
    defineParameter("historicalFireYears", "numeric", 1991:2022, NA, NA,
                    paste("range of years captured by the historical climate data")),
    defineParameter("outputDir", "character", NA_character_, NA, NA,
                    paste("Directory path for prepared climate data rasters.",
                          "User should specify a shared directory",
                          "(e.g., when running multiple simulation replicates)",
                          "to avoid multiple copies of the climate data.",
                          "If not specified by the user, a subdirectory of the simulation output",
                          "directory will be used (i.e., `file.path(outputPath(sim), 'climate')`.")),
    defineParameter("projectedFireYears", "numeric", 2011:2100, NA, NA,
                    paste("range of years captured by the projected climate data")),
    defineParameter("projectedType", "character", "forecast", NA, NA,
                    paste("one of 'forecast' or 'hindcast' to prepare either projected future,",
                          "or sampled historical data for hindcast studies, respectively.")),
    defineParameter("quickCheck", "logical", TRUE, NA, NA,
                    paste("`prepClimateData` uses `prepInputs` internally; set this to `TRUE` to avoid",
                          "the slow process of digesting potentially MANY files.",
                          "This will use `file.size` only, if `TRUE`.")),
    defineParameter("studyAreaName", "character", NA_character_, NA, NA,
                    paste("User-defined label for the current stuyd area.",
                          "If `NA`, a hash of `studyArea` will be used.")),
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
    expectsInput("studyArea", "sf",
                 desc = "study area used for simulation (buffered to mitigate edge effects)",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
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
  stopifnot(P(sim)$projectedType %in% c("forecast", "hindcast"))

  ## separate intermediate outputs from raw inputs, to reduce file conflicts on shared drives
  climatePath <- file.path(inputPath(sim), "climate") |> checkPath(create = TRUE) |> asPath(1)
  climatePathOut <- if (is.null(P(sim)$outputDir) || is.na(P(sim)$outputDir)) {
    file.path(outputPath(sim), "climate") |> checkPath(create = TRUE) |> asPath(1)
  } else {
    file.path(P(sim)$outputDir) |> checkPath(create = TRUE) |> asPath(1)
  }

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
  projected_yrs <- P(sim)$projectedFireYears

  GCM <- P(sim)$climateGCM
  SSP <- P(sim)$climateSSP

  if (P(sim)$projectedType == "forecast") {
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
      projected_ATA = list(
        vars = c("future_MAT", "historic_MAT_normal"),
        fun = quote(calcATA),
        .dots = list(historic_period = historic_prd, future_years = projected_yrs)
      ),
      projected_CMI = list(
        vars = "future_CMI",
        fun = quote(calcAsIs),
        .dots = list(future_years = projected_yrs)
      ),
      projected_MDC = list(
        vars = c(sprintf("future_PPT%02d", 4:9), sprintf("future_Tmax%02d", 4:9)),
        fun = quote(calcMDC),
        .dots = list(future_years = projected_yrs)
      )
    )
  } else if (P(sim)$projectedType == "hindcast") {
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
      ## projected climate variables will be prepared from historic and sampled below
      projected_ATA = list(
        vars = c("historic_MAT", "historic_MAT_normal"),
        fun = quote(calcATA),
        .dots = list(historic_period = historic_prd, historic_years = historic_yrs)
      ),
      projected_CMI = list(
        vars = "historic_CMI",
        fun = quote(calcAsIs),
        .dots = list(historic_years = historic_yrs)
      ),
      projected_MDC = list(
        vars = c(sprintf("historic_PPT%02d", 4:9), sprintf("historic_Tmax%02d", 4:9)),
        fun = quote(calcMDC),
        .dots = list(historic_years = historic_yrs)
      )
    )
  }

  climateRasters <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    historic_years = historic_yrs,
    future_years = projected_yrs,
    historic_period = historic_prd,
    future_period = NULL,
    gcm = GCM,
    ssp = SSP,
    cl = NULL, ## TODO: allow user to pass their own cl object to module
    studyArea = sim$studyArea,
    studyAreaName = P(sim)$studyAreaName,
    rasterToMatch = sim$rasterToMatch
  ) |>
    Cache(omitArgs = c("climatePath", "climatePathOut")) ## TODO: improve use of cache

  ## rename list elements and layers to match expected names downstream
  names(climateRasters) <- names(climateVariables)

  names(climateRasters$historic_CMI_normal) |>
    gsub("CMI_normal_", "period", x = _) |>
    terra::set.names(climateRasters$historic_CMI_normal, value = _)

  names(climateRasters$historic_MDC) |>
    gsub("MDC_historic_", "year", x = _) |>
    terra::set.names(climateRasters$historic_MDC, value = _)

  names(climateRasters$projected_ATA) |>
    gsub("ATA_future_|ATA_historic_", "year", x = _) |>
    terra::set.names(climateRasters$projected_ATA, value = _)

  names(climateRasters$projected_CMI) |>
    gsub("CMI_future_|CMI_historic_", "year", x = _) |>
    terra::set.names(climateRasters$projected_CMI, value = _)

  names(climateRasters$projected_MDC) |>
    gsub("MDC_future_|MDC_historic_", "year", x = _) |>
    terra::set.names(climateRasters$projected_MDC, value = _)

  ## sample use historic layers for use with hindcasting
  if (P(sim)$projectedType == "hindcast") {
    ## use same (sampled) years for each climate variable!
    rndsmp <- sample(x = seq(terra::nlyr(climateRasters$historic_MDC)),
                     size = length(projected_yrs),
                     replace = TRUE)

    climateRasters$projected_ATA <- climateRasters$projected_ATA[[rndsmp]]
    terra::set.names(climateRasters$projected_ATA, paste0("year", projected_yrs))

    climateRasters$projected_CMI <- climateRasters$projected_CMI[[rndsmp]]
    terra::set.names(climateRasters$projected_CMI, paste0("year", projected_yrs))

    climateRasters$projected_MDC <- climateRasters$projected_MDC[[rndsmp]]
    terra::set.names(climateRasters$projected_MDC, paste0("year", projected_yrs))
  }

  ## save rasters to disk with updated layers names

  ## TODO: parallelize this so it's faster? wrapping SpatRasters is [too] slow here?
  climateRasters <- lapply(climateRasters, function(x) {
    terra::writeRaster(x, .suffix(terra::sources(x), "updated"), overwrite = TRUE)
  })

  # climateRastersWrapped <- lapply(climateRasters, terra::wrap)
  # climateRasters <- parallel::parLapply(cl, climateRastersWrapped, function(x) {
  #   terra::writeRaster(x, .suffix(terra::sources(x), "updated"), overwrite = TRUE)
  # })
  # climateRasters <- lapply(climateRasters, terra::unwrap)

  ## objects for fireSense and LandR.CS:
  sim$historicalClimateRasters <- list(MDC = climateRasters$historic_MDC,
                                       CMI_normal = climateRasters$historic_CMI_normal)
  sim$projectedClimateRasters <- list(MDC = climateRasters$future_MDC,
                                      ATA = climateRasters$future_ATA,
                                      CMI = climateRasters$future_CMI)

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- inputPath(sim) |> asPath(1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  ## TODO: ensure defaults work using terra/sf
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  if (!suppliedElsewhere("studyArea", sim)) {
    ## random study area spanning portions of NE AB and NW SK
    sim$studyArea <- terra::vect(cbind(-110, 58), crs = "epsg:4326") |>
      SpaDES.tools::randomStudyArea(size = 5e10) |>
      sf::st_as_sf() |>
      sf::st_transform(mod$targetCRS) |>
      sf::st_buffer(P(sim)$bufferDist)
  }

  if (is.na(P(sim)$studyAreaName)) {
    ## use unique hash as study area name
    P(sim, "studyAreaName") <- studyAreaName(sim$studyArea)
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                               year = 2005,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               useCache = P(sim)$.useCache,
                               filename2 = NULL)
    writeRaster(sim$rasterToMatch, file.path(dPath, paste0(P(sim)$studyAreaName, "_rtm.tif")),
                datatype = "INT1U", overwrite = TRUE)
  }

  return(invisible(sim))
}

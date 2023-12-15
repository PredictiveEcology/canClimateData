## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process historic, projected, and normals data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# load packages -------------------------------------------------------------------------------

library(archive)
library(future)
library(future.apply)
library(future.callr)
# library(geodata)
# library(googledrive)
# library(nngeo)
library(sf)
library(terra)

library(reproducible)

library(climateData)

# setup ---------------------------------------------------------------------------------------

## adjust these values for your user / machine

createTiles <- FALSE # TRUE

dPath <- "data"

googledrive::drive_auth(email = "achubaty@for-cast.ca")

# create tiles --------------------------------------------------------------------------------

if (isTRUE(createTiles)) {
  targetCRS <- "epsg:4326" ## ClimateNA needs lat/lon, and the DEM already uses this

  canada <- geodata::gadm(country = "CAN", level = 0, path = dPath, version = "4.1", resolution = 1) |>
    st_as_sf() |>
    st_transform(targetCRS) |>
    nngeo::st_remove_holes()

  tiles <- st_make_grid(canada, n = c(10, 10))
  gpkg <- file.path(dPath, "tiles.gpkg")
  unlink(gpkg)
  st_write(tiles, gpkg)

  idx <- st_intersects(tiles, canada) |>
    lapply(any) |>
    unlist() |>
    which()

  if (interactive()) {
    plot(tiles)
    plot(tiles[idx], col = "lightgrey", add = TRUE)
    plot(st_geometry(canada), add = TRUE)
  }

  arcSecRes <- c(60, 60) ## .asc format needs these to be the same
  gtopo30N <- prepInputs(
    url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
    destinationPath = dPath#,
    #cropTo = canada,
    #maskTo = canada ## hangs during mask
  ) |>
    project(targetCRS, res = arcSecRes/60/60) |>
    crop(canada) |>
    mask(canada) |>
    trim()

  if (interactive()) {
    plot(gtopo30N, legend = FALSE)
    plot(tiles[idx], add = TRUE, border = "blue", lwd = 2)
    st_centroid(tiles[idx]) |>
      st_coordinates() |>
      text(col = "blue")
  }

  dem_ff <- makeTiles(
    x = gtopo30N,
    y = vect(tiles[idx]),
    filename = file.path(dPath, "can_dem_.asc"),
    na.rm = TRUE
  )

  ## NOTE: rewrite line endings in asc files for windows
  lapply(dem_ff, function(f) {
    rewrite_asc(f)
  })

  if (interactive()) {
    ## visually check that the raster ids match the polygon ids for the tiles
    dem <- vrt(dem_ff)
    plot(dem, legend = FALSE)
    plot(tiles, add = TRUE)

    lapply(dem_ff, function(f) {
      id <-  tileID(f)
      r <- rast(f)
      text(cbind(xmin(r) + (xmax(r) - xmin(r)) / 2, ymin(r) + (ymax(r) - ymin(r)) / 2), id)

      return(id)
    })

    rm(vrt)
  }
}

# use ClimateNA to fetch and process climate data ---------------------------------------------

## TODO: currently, the package is of very low quality,
##       offering little more than a clumsy wrapper around system2()

# if (!"ClimateNAr" %in% row.names(installed.packages())) {
#   dlDir <- file.path("C:/Users", Sys.info()[["user"]], "Downloads")
#   pkgurl <- "https://climatena.ca/downloads/ClimateNAr_1.2.0.zip"
#   pkglcl <- file.path(dlDir, basename(pkgurl))
#   download.file(pkgurl, pkglcl)
#   install.packages(pkglcl, repos = NULL)
# }
#
# library(ClimateNAr)

ClimateNAdir <- "C:/Climatena_v742"
ClimateNAexe <- "ClimateNA_v7.42.exe"
ClimateNAdata <- "C:/ClimateNA_data"

plan("callr", workers = parallelly::availableCores())

if (!exists("dem_ff")) {
  dem_ff <- list.files(dPath, pattern = "[.]asc$", full.names = TRUE)
}

# get ClimateNA normals -----------------------------------------------------------------------

MSYs <- c("Y")
period_nrm <- c(
  "Normal_1901_1930.nrm",
  "Normal_1911_1940.nrm",
  "Normal_1921_1950.nrm",
  "Normal_1931_1960.nrm",
  "Normal_1941_1970.nrm",
  "Normal_1951_1980.nrm", ## LandR.CS/fireSense
  "Normal_1971_2000.nrm",
  "Normal_1981_2010.nrm", ## LandR.CS/fireSense
  "Normal_1991_2020.nrm"
)

future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(MSYs, function(msy) {
    lapply(period_nrm, function(nrm) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "normals")

      withr::local_dir(ClimateNAdir)
      system2(ClimateNAexe,
              args = c(
                paste0("/", msy),
                paste0("/", nrm),
                paste0("/", f),
                paste0("/", ClimateNAout)
              ))
      withr::deferred_run()
    })
  })
})

# get ClimateNA historic time series ----------------------------------------------------------

MSYs <- c("MSY", "M", "Y")
# period_ann <- paste0("Year_", 1901:1990, ".ann")
period_ann <- paste0("Year_", 1991:2022, ".ann")

future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(MSYs, function(msy) {
    lapply(period_ann, function(ann) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "historic", msy)

      withr::local_dir(ClimateNAdir)
      system2(ClimateNAexe,
              args = c(
                paste0("/", msy),
                paste0("/", ann),
                paste0("/", f),
                paste0("/", ClimateNAout)
              ))
      withr::deferred_run()
    })
  })
})

# get ClimateNA future time series ------------------------------------------------------------

MSYs <- c("MSY", "M", "Y")
GCMs <- c("CanESM5", "CNRM-ESM2-1")
SSPs <- c("370", "585")
years <- 2011:2100

future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "future", msy, gcm, ssp)

        lapply(years, function(yr) {
          withr::local_dir(ClimateNAdir)
          system2(ClimateNAexe,
                  args = c(
                    paste0("/", msy),
                    paste0("/", gcm, "_ssp", ssp, "@", yr, ".gcm"),
                    paste0("/", f),
                    paste0("/", ClimateNAout)
                  ))
          withr::deferred_run()
        })
      })
    })
  })
})

# archive tilesets ----------------------------------------------------------------------------

## TODO: reduce number of workers using tweak()

## normals

MSYs <- c("Y")
period_nrm <- c("Normal_1951_1980.nrm", "Normal_1981_2010.nrm")

zip_normals <- future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(MSYs, function(msy) {
    lapply(period_nrm, function(nrm) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "normals")
      fzip <- paste0(ClimateNAout, "_normals.zip")

      archive_write_dir(archive = fzip, dir = ClimateNAout)
    })
  })
})

## historic time series

MSYs <- c("MSY", "M", "Y")
period_ann <- paste0("Year_", 1991:2022, ".ann")

future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(MSYs, function(msy) {
    lapply(period_ann, function(ann) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "historic", msy)
      fzip <- paste0(ClimateNAout, "_", msy, ".zip")

      archive_write_dir(archive = fzip, dir = ClimateNAout)
    })
  })
})

## future time series

MSYs <- c("MSY", "M", "Y")
GCMs <- c("CanESM5", "CNRM-ESM2-1")
SSPs <- c("370", "585")

future_lapply(dem_ff, function(f) {
  f <- normalizePath(f)

  lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "future", msy, gcm, ssp)
        fzip <- paste0(ClimateNAout, gcm, "_", ssp, "_", tileID(f), "_", tileID(f), "_", msy, ".zip")

        archive_write_dir(archive = fzip, dir = ClimateNAout)
      })
    })
  })
})


# upload tilesets -----------------------------------------------------------------------------

## TODO

# cleanup -------------------------------------------------------------------------------------

terra::tmpFiles(remove = TRUE)

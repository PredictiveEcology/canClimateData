# load packages -------------------------------------------------------------------------------

## archives and up/downloads
library(archive)
# library(googledrive)

## database
library(DBI)
library(dplyr)
#library(RSQLite)

## parallel processing
library(future)
library(future.apply)
library(future.callr)

## geospatial
# library(geodata)
# library(nngeo)
library(sf)
library(terra)

library(reproducible)

library(climateData)

# setup ---------------------------------------------------------------------------------------

## adjust these values for your user / machine

ClimateNAdir <- "C:/Climatena_v742"
ClimateNAexe <- "ClimateNA_v7.42.exe"
ClimateNAdata <- switch(Sys.info()[["sysname"]],
                        Linux = "/mnt/data/climate/ClimateNA_data",
                        Windows = "C:/ClimateNA_data")
stopifnot(dir.exists(ClimateNAdata))

dPath <- file.path("data")

userEmail <- "achubaty@for-cast.ca"
oauthCachePath <- file.path(".secrets")
googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

## database tracks which data already processed / uploaded

primaryDBfile <- file.path(ClimateNAdata, "ClimateNA_tiles.sqlite")
pkgDBfile <- file.path("~/GitHub/PredictiveEcology/climateData/inst/extdata", basename(primaryDBfile))
tempDBfile <- tempfile(fileext = ".sqlite")

if (file.exists(primaryDBfile)) {
  file.copy(primaryDBfile, tempDBfile) ## always work on a copy; primary on network drive 'locked'.
}

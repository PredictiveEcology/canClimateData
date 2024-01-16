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

dPath <- "data"

googledrive::drive_auth(email = "achubaty@for-cast.ca")

## database tracks which data already processed / uploaded

primaryDBfile <- file.path(ClimateNAdata, "ClimateNA_tiles.sqlite")
tempDBfile <- tempfile(fileext = ".sqlite")

if (file.exists(primaryDBfile)) {
  file.copy(primaryDBfile, tempDBfile) ## always work on a copy; primary on network drive 'locked'.
}

ClimateNA_sql <- function(dbfile, type) {
  firstRun <- file.exists(dbfile)

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbfile,
    synchronous = "normal",
    extended_types = TRUE ## for DATETIME
  )

  if (isTRUE(firstRun)) {
    DBI::dbExecute(db, "PRAGMA journal_mode = WAL")   ## using WAL allows concurrency
    DBI::dbExecute(db, "PRAGMA busy_timeout = 60000") ## set busy timeout (ms); allows concurrency.
  }

  df_template <- switch(
    type,
    historic_normals = data.frame(
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      period = NA_character_,  ## climate period
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    future_normals = data.frame(
      gcm = NA_character_,     ## climate scenario GCM
      ssp = NA_character_,     ## climate scenario SSP
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      period = NA_character_,  ## climate period
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    historic = data.frame(
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      year = NA_character_,    ## climate year
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    future = data.frame(
      gcm = NA_character_,     ## climate scenario GCM
      ssp = NA_character_,     ## climate scenario SSP
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      year = NA_character_,    ## climate year (or period)
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    )
  ) |>
    tibble::rowid_to_column() |> ## add rowid column to use as table primary key
    na.omit()

  tbl <- type

  if (!DBI::dbExistsTable(db, tbl)) {
    DBI::dbCreateTable(db, tbl, df_template)
  }

  df <- dplyr::tbl(db, tbl)

  return(list(db = db, df = df))
}

source("scripts/01-ClimateNA_setup.R")

# create tiles --------------------------------------------------------------------------------

targetCRS <- "epsg:4326" ## ClimateNA needs lat/lon, and the DEM already uses this

canada <- geodata::gadm(country = "CAN", level = 0, path = dPath, version = "4.1", resolution = 1) |>
  st_as_sf() |>
  st_transform(targetCRS) |>
  nngeo::st_remove_holes()

tiles <- st_make_grid(canada, n = c(10, 10)) |>
  dplyr::mutate(id = dplyr::row_number(), .before = geom)
st_write(tiles, file.path(dPath, "tiles.gpkg"))

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
  filename = file.path(file.path(ClimateNAdata, "dem"), "can_dem_.asc"),
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

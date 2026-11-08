
# Setup -------------------------------------------------------------------

library(tidyverse)
library(arcgislayers)
library(sf)
library(terra)

options(timeout = 600)

utm18n <- st_crs(26918)

file_path <- "C:\\Users\\albin\\OneDrive\\Documents\\ArcGIS\\Projects\\fire-towers\\"

file_gdb <- glue::glue("{file_path}fire-towers.gdb")

# Get Data ----------------------------------------------------------------

# Towers
tower_heights <- read_csv(here::here("data", "day05", "catskill_fire_towers.csv"))

towers_url <- "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/NYS_State_Land_Assets/FeatureServer/31"
towers_layer <- arc_open(towers_url)

print(towers_layer)

fields <- list_fields(towers_layer)
fields$name


all_towers <- arc_select(
  towers_layer,
  fields = c("OBJECTID", "FACILITY", "NAME", "ASSET", "PUBLICUSE", "ACCESSIBLE")
) |> 
  janitor::clean_names()

catskill_towers <- tower_heights |> 
  left_join(all_towers, by = c("layer_name" = "name")) |> 
  st_as_sf() |> 
  st_transform(utm18n)

st_write(
  catskill_towers, 
  file_gdb,
  layer = "fire_towers", 
  driver = "OpenFileGDB", 
  delete_layer = TRUE
)

# Park boundary
 
boundary_url <- "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/Catskill_Park_Boundary/FeatureServer/7"
boundary_layer <- arc_open(boundary_url)
print(boundary_layer)

boundary_fields <- list_fields(boundary_layer)
boundary_fields$name

park_boundary <- arc_select(
  boundary_layer
) |> 
  janitor::clean_names() |> 
  st_as_sf() |> 
  st_transform(utm18n)

st_write(
  park_boundary, 
  file_gdb,
  layer = "boundary", 
  driver = "OpenFileGDB",
  delete_layer = TRUE
)



# DEM service


# DEM service: statewide 10 m (stored in NAD27, so we ask the server for NAD83 UTM 18N)
dem_url <- "https://elevation.its.ny.gov/arcgis/rest/services/USGS_10_meter/ImageServer"
dem_layer <- arc_open(dem_url)

# Area of interest: park boundary + 40 km, in meters
aoi <- park_boundary |>
  st_transform(26918) |>
  st_buffer(40000) |>
  st_bbox()

# Snap the box to the 10 m grid so the tiles line up exactly
cell <- 10
xmin <- floor(aoi[["xmin"]] / cell) * cell
xmax <- ceiling(aoi[["xmax"]] / cell) * cell
ymin <- floor(aoi[["ymin"]] / cell) * cell
ymax <- ceiling(aoi[["ymax"]] / cell) * cell

# Split into tiles of 4,000 x 4,000 pixels (the service caps requests at 15,000 x 4,100)
tile_size <- 4000 * cell
x_breaks <- unique(c(seq(xmin, xmax, by = tile_size), xmax))
y_breaks <- unique(c(seq(ymin, ymax, by = tile_size), ymax))

tiles <- expand_grid(
  i = seq_len(length(x_breaks) - 1),
  j = seq_len(length(y_breaks) - 1)
) |>
  mutate(
    xmin = x_breaks[i], xmax = x_breaks[i + 1],
    ymin = y_breaks[j], ymax = y_breaks[j + 1]
  ) |>
  select(xmin, xmax, ymin, ymax)

tile_dir <- here::here("data", "day05", "dem_tiles")
dir.create(tile_dir, showWarnings = FALSE)

# Download each tile straight to disk (skips tiles already saved)
tile_files <- pmap_chr(
  tiles |> mutate(id = row_number()),
  \(xmin, xmax, ymin, ymax, id) {
    out <- file.path(tile_dir, sprintf("tile_%02d.tif", id))
    if (!file.exists(out)) {
      arc_raster(
        dem_layer,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        bbox_crs = utm18n,
        crs = utm18n,
        width = (xmax - xmin) / cell,
        height = (ymax - ymin) / cell
      ) |>
        writeRaster(out, overwrite = TRUE)
    }
    out
  }
)

# Stitch on disk, not in memory
dem <- vrt(tile_files)

writeRaster(dem, glue::glue("{file_path}dem_10.tif"), overwrite = TRUE)

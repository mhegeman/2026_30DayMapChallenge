
# Setup -------------------------------------------------------------------

library(tidyverse)
library(arcgislayers)
library(sf)
library(terra)
library(elevatr)
library(ggrepel)
library(ggtext)
library(showtext)

options(timeout = 600)

utm18n <- st_crs(26918)

# Old Standard is modeled on late-1800s/early-1900s book type, close to the
# Roman lettering on early USGS quads
font_add_google("Old Standard TT", "oldstd")
showtext_auto()
showtext_opts(dpi = 300)

bsky_logo_path <- here::here("img", "bsky_logo.png")


# Get Data ----------------------------------------------------------------

# Towers
towers <- read_csv(here::here("data", "day06", "catskill_fire_towers_history.csv"))

towers_sf <- towers |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(utm18n)

# Park boundary

boundary_url <- "https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/Catskill_Park_Boundary/FeatureServer/7"
boundary_layer <- arc_open(boundary_url)
# print(boundary_layer)
#
# boundary_fields <- list_fields(boundary_layer)
# boundary_fields$name

park_boundary <- arc_select(
  boundary_layer
) |>
  janitor::clean_names() |>
  st_as_sf() |>
  st_transform(utm18n)


# Tower groups ------------------------------------------------------------

# Two groups for the map: Standing and Removed. Sites a tower moved away from
# are dropped: the two relocated towers (Gallis Hill, Gilbert Lake) are already
# on the map at their new summits (Overlook, Leonard Hill), and the Hunter
# Mountain "previous site" sits only ~330 m from the summit tower.
# Bramley follows the brochure (removed 1975). See the notes column.
towers_map <- towers_sf |>
  filter(!fate %in% c("moved", "relocated")) |>
  mutate(
    group = if_else(fate == "removed", "Removed", "Standing"),
    short_name = tower |>
      str_replace(" Mountain$", " Mt.") |>
      str_replace("^Mount ", "Mt. "),
    years = if_else(
      group == "Standing",
      as.character(built),
      glue::glue("{built}–{fate_year}")
    ),
    label = glue::glue("{short_name}\n{years}")
  )

towers_map |> st_drop_geometry() |> count(group)   # check the groups


# Map extent --------------------------------------------------------------

# Every tower plus a 12 km margin, so the far-west towers fit
aoi <- towers_map |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(12000, joinStyle = "MITRE") |>
  st_bbox()


# Towns for context -------------------------------------------------------

# NYS Civil Boundaries: layer 4 = cities, layer 7 = villages (polygons).
# Woodstock and Phoenicia are hamlets, so they aren't in either layer.
civil_url <- "https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Civil_Boundaries/FeatureServer/"

cities <- arc_select(
  arc_open(paste0(civil_url, 4)),
  where = "NAME IN ('Kingston', 'Oneonta')",
  fields = "NAME"
)

villages <- arc_select(
  arc_open(paste0(civil_url, 7)),
  where = "NAME IN ('Cobleskill', 'Delhi', 'Walton', 'Margaretville', 'Monticello', 'Ellenville', 'New Paltz')",
  fields = "NAME"
)

towns <- bind_rows(cities, villages) |>
  janitor::clean_names() |>
  st_transform(utm18n) |>
  st_point_on_surface() |>   # one point inside each town polygon
  st_crop(aoi)               # drop any that fall outside the map frame


# Elevation ---------------------------------------------------------------

# Coarse DEM for the whole region from AWS Terrain Tiles via elevatr.
# z = 10 is roughly 110 m cells here. Saved to disk so it only downloads once.
dem_file <- here::here("data", "day06", "dem_region.tif")

if (!file.exists(dem_file)) {
  aoi_poly <- st_as_sf(st_as_sfc(aoi))

  dem_dl <- get_elev_raster(aoi_poly, z = 10, clip = "bbox") |>
    rast()

  writeRaster(dem_dl, dem_file, overwrite = TRUE)
}

dem <- rast(dem_file) |>
  project("EPSG:26918", res = 100) |>
  crop(ext(aoi["xmin"], aoi["xmax"], aoi["ymin"], aoi["ymax"]))

# Old quads are in feet
dem_ft <- dem * 3.281
dem_ft[dem_ft < 0] <- 0   # tiles can dip below 0 over water


# Hillshade ---------------------------------------------------------------

slope  <- terrain(dem, "slope",  unit = "radians")
aspect <- terrain(dem, "aspect", unit = "radians")

hillshade <- shade(slope, aspect, angle = 40, direction = 315)

hill_df <- as.data.frame(hillshade, xy = TRUE) |>
  rename(shade = 3)


# Contours ----------------------------------------------------------------

# Only every 1,000 ft, traced from a smoothed 300 m copy of the DEM so the
# lines are calm enough to read the towers through
dem_smooth <- dem_ft |>
  aggregate(fact = 3) |>
  focal(w = 3, fun = "mean", na.rm = TRUE)

contours <- as.contour(dem_smooth, levels = c(1000, 2000, 3000, 4000)) |>
  st_as_sf()


# Colors ------------------------------------------------------------------

paper_col   <- "#EFE6CF"   # aged cream paper
ink_col     <- "#2B2620"   # black ink, slightly warm
contour_col <- "#A0703F"   # USGS contour brown
shade_col   <- "#5A4632"   # hillshade ink
lost_col    <- "#B1361E"   # topo red

group_cols   <- c("Standing" = ink_col, "Removed" = lost_col, "Town" = ink_col)
group_shapes <- c("Standing" = 17, "Removed" = 2)   # filled / open triangle


# Text --------------------------------------------------------------------

n_total     <- nrow(towers_map)
n_removed   <- sum(towers_map$group == "Removed")
n_restored  <- sum(towers_map$fate == "restored")

title_text <- "FIRE TOWERS OF THE CATSKILLS"

subtitle_text <- glue::glue(
  "{n_total} towers once monitored the Catskill region for smoke. ",
  "<span style='color:{lost_col}'>{n_removed} have been torn down</span>.<br>",
  "{n_restored} inside Catskill Park are restored and open to hikers."
)


# Labels ------------------------------------------------------------------

# Towers and towns share one label layer so ggrepel keeps them apart.
# Old topo convention: towns in upright type, features in italics.
labels <- bind_rows(
  towers_map |> transmute(label, group, face = "italic"),
  towns      |> transmute(label = name, group = "Town", face = "plain")
)

caption_text <- glue::glue(
  "Data: Forest Fire Lookout Association, NYS DEC, NYS ITS, AWS Terrain Tiles via elevatr<br>",
  "#30DayMapChallenge 2026 Day 06 – Vintage<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)


# Map ---------------------------------------------------------------------

p1 <- ggplot() +
  # Hillshade: darker where the slope faces away from the light
  geom_raster(
    data = hill_df,
    aes(x, y, alpha = 1 - shade),
    fill = shade_col
  ) +
  scale_alpha(range = c(0, 0.45), guide = "none") +
  # Contours (1,000 ft)
  geom_sf(
    data = contours,
    color = contour_col, linewidth = 0.3, alpha = 0.6
  ) +
  # Park boundary
  geom_sf(
    data = park_boundary,
    fill = NA, color = ink_col, linewidth = 0.4, linetype = "22"
  ) +
  # Towns: small open circle with a center dot, like an old quad
  geom_sf(data = towns, shape = 21, size = 2, stroke = 0.5,
          color = ink_col, fill = paper_col) +
  geom_sf(data = towns, shape = 16, size = 0.5, color = ink_col) +
  # Towers
  geom_sf(
    data = towers_map,
    aes(color = group, shape = group),
    size = 2.4, stroke = 0.8
  ) +
  geom_text_repel(
    data = labels,
    aes(label = label, geometry = geometry, color = group, fontface = face),
    stat = "sf_coordinates",
    family = "oldstd", size = 2.6, lineheight = 0.9,
    bg.color = paper_col, bg.r = 0.15,   # paper-colored halo so labels read over contours
    min.segment.length = 0.3, segment.size = 0.2, box.padding = 0.4,
    seed = 6
  ) +
  scale_color_manual(values = group_cols, guide = "none") +
  scale_shape_manual(values = group_shapes, guide = "none") +
  coord_sf(
    crs = utm18n,
    datum = 4326,   # degree labels on the frame, like a quad's corners
    xlim = c(aoi["xmin"], aoi["xmax"]),
    ylim = c(aoi["ymin"], aoi["ymax"]),
    expand = FALSE
  ) +
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  theme_void(base_family = "oldstd") +
  theme(
    plot.background  = element_rect(fill = paper_col, color = NA),
    panel.background = element_rect(fill = paper_col, color = NA),
    # Neatline around the map, with tick marks and degree labels outside it
    panel.border = element_rect(fill = NA, color = ink_col, linewidth = 0.8),
    axis.text    = element_text(size = 6.5, color = ink_col),
    axis.ticks   = element_line(color = ink_col, linewidth = 0.3),
    axis.ticks.length = unit(-1.5, "mm"),
    plot.title = element_text(
      size = 22, color = ink_col, hjust = 0.5,
      margin = margin(t = 6, b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 10.5, color = ink_col, hjust = 0.5, halign = 0.5,
      lineheight = 1.3, margin = margin(b = 12)
    ),
    plot.caption = element_markdown(
      size = 7.5, color = "grey35", lineheight = 1.2,
      margin = margin(t = 10)
    ),
    plot.margin = margin(18, 22, 14, 22)
  )


# plot(p1)

# With showtext dpi = 300, the plot pane shows text far too big. Judge the PNG.
ggsave(here::here("output", "day06_vintage.png"), p1,
       width = 9, height = 10, dpi = 300, bg = paper_col)


# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)
library(ggtext)
library(showtext)

sf_use_s2(TRUE)   # spherical geometry for areas, distances and the hemisphere clip

font_add_google("Source Sans 3", "src")
showtext_auto()
showtext_opts(dpi = 300)

bsky_logo_path <- here::here("img", "bsky_logo.png")


# Settings ----------------------------------------------------------------

# One 10' x 10' cell (1/6 of a degree on each side) at each latitude, all on
# the same meridian so the shrinking reads straight up the globe.
# Each latitude is the cell's southern edge.
cell_lats <- c(0, 30, 60, 85)
cell_lon  <- -74      # Manhattan's meridian
cell_size <- 10 / 60  # 10 minutes of arc, in degrees

# Globe center: far enough north that both the equator and 85°N are visible
globe_lon <- cell_lon
globe_lat <- 40


# 10' cells ---------------------------------------------------------------

make_cell <- function(lat, lon = cell_lon, size = cell_size) {
  st_bbox(c(xmin = lon, xmax = lon + size, ymin = lat, ymax = lat + size),
          crs = st_crs(4326)) |>
    st_as_sfc() |>
    st_segmentize(units::set_units(500, m))   # extra vertices so edges curve when projected
}

cells <- tibble(lat = cell_lats) |>
  mutate(
    geometry = map(lat, make_cell) |> do.call(what = c),
    mid_lat  = lat + cell_size / 2,
    mid_lon  = cell_lon + cell_size / 2
  ) |>
  st_as_sf(crs = 4326)

# Real sizes on the ground (s2 uses a sphere, so these are close, not survey-exact)
pt <- function(lon, lat) st_sfc(st_point(c(lon, lat)), crs = 4326)

cells <- cells |>
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    ns_km = map_dbl(lat, \(l) as.numeric(st_distance(
      pt(cell_lon, l), pt(cell_lon, l + cell_size))) / 1000),
    ew_km = map_dbl(mid_lat, \(l) as.numeric(st_distance(
      pt(cell_lon, l), pt(cell_lon + cell_size, l))) / 1000),
    lat_label = if_else(lat == 0, "Equator", paste0(lat, "°N"))
  )

cells |> st_drop_geometry()   # check the numbers


# Globe -------------------------------------------------------------------

ortho <- st_crs(glue::glue(
  "+proj=ortho +lat_0={globe_lat} +lon_0={globe_lon} +a=6371000 +b=6371000 +units=m +no_defs"
))

# The visible hemisphere as a polygon on the sphere. Clipping to it before
# projecting stops land on the far side from turning into stray polygons.
# 9,800 km is just under a quarter of the way around the Earth.
hemisphere <- pt(globe_lon, globe_lat) |>
  st_buffer(9.8e6, max_cells = 5000)

# Countries come from the s2 package (Natural Earth 1:110m, already cleaned
# for spherical geometry). rnaturalearth's copy has Russia and Sudan polygons
# that s2 rejects, even after st_make_valid().
land <- s2::s2_data_countries() |>
  st_as_sfc() |>
  st_union() |>
  st_intersection(hemisphere) |>
  st_transform(ortho)

graticule <- st_graticule(lon = seq(-180, 160, 20), lat = seq(-80, 80, 20)) |>
  st_geometry() |>
  st_intersection(hemisphere) |>
  st_transform(ortho)

# Ocean disc: the whole visible face of the globe
ocean <- st_sfc(st_point(c(0, 0)), crs = ortho) |>
  st_buffer(6371000)

# The cells are far too small to see at this scale, so mark them with points
markers <- cells |>
  st_drop_geometry() |>
  st_as_sf(coords = c("mid_lon", "mid_lat"), crs = 4326) |>
  st_transform(ortho)

# The meridian the cells sit on, from the equator to the pole
meridian <- st_linestring(cbind(cell_lon, seq(0, 90, 1))) |>
  st_sfc(crs = 4326) |>
  st_transform(ortho)


# Colors ------------------------------------------------------------------

bg_col    <- "#F7F5F0"
ocean_col <- "#DCE6EC"
land_col  <- "#C9C3B6"
grid_col  <- "white"
ink_col   <- "#22303A"
cell_col  <- "#D1495B"


# Globe plot --------------------------------------------------------------

p_globe <- ggplot() +
  geom_sf(data = ocean, fill = ocean_col, color = ink_col, linewidth = 0.3) +
  geom_sf(data = graticule, color = grid_col, linewidth = 0.25) +
  geom_sf(data = land, fill = land_col, color = NA) +
  geom_sf(data = meridian, color = cell_col, linewidth = 0.4, linetype = "22") +
  geom_sf(data = markers, color = cell_col, size = 2.5) +
  geom_sf_text(
    data = markers, aes(label = lat_label),
    family = "src", size = 3.2, color = ink_col,
    hjust = 0, nudge_x = 250000   # 250 km to the right of each point
  ) +
  coord_sf(crs = ortho, datum = NA) +
  theme_void(base_family = "src")


# Cell panels -------------------------------------------------------------

# Every panel uses the same scale, so the cells can be compared directly.
# Each cell is drawn in its own equal-area projection centered on it, so its
# shape and size are true. The equator cell is drawn as a dashed outline in
# every panel for comparison.
half_width <- 11000   # metres from the panel center to each edge

laea_for <- function(lat, lon) {
  st_crs(glue::glue("+proj=laea +lat_0={lat} +lon_0={lon} +datum=WGS84 +units=m +no_defs"))
}

# Equator cell outline as plain x/y, relative to its own center
equator <- cells |> filter(lat == min(lat))
ghost <- equator |>
  st_transform(laea_for(equator$mid_lat, equator$mid_lon)) |>
  st_coordinates() |>
  as_tibble()

make_panel <- function(i) {
  cell <- cells[i, ]
  crs_i <- laea_for(cell$mid_lat, cell$mid_lon)

  ggplot() +
    geom_path(data = ghost, aes(X, Y),
              color = ink_col, linewidth = 0.3, linetype = "22") +
    geom_sf(data = st_transform(cell, crs_i),
            fill = cell_col, color = cell_col, alpha = 0.85) +
    coord_sf(
      crs = crs_i, datum = NA,
      xlim = c(-half_width, half_width),
      ylim = c(-half_width, half_width),
      expand = FALSE
    ) +
    labs(
      title    = cell$lat_label,
      subtitle = glue::glue(
        "{scales::number(cell$ew_km, accuracy = 0.1)} × ",
        "{scales::number(cell$ns_km, accuracy = 0.1)} km · ",
        "{scales::comma(cell$area_km2)} km²"
      )
    ) +
    theme_void(base_family = "src") +
    theme(
      plot.title    = element_text(size = 10, face = "bold", color = ink_col),
      plot.subtitle = element_text(size = 8, color = ink_col),
      plot.margin   = margin(4, 4, 4, 4)
    )
}

# Pole at the top, equator at the bottom, to match the globe
panels <- map(rev(seq_len(nrow(cells))), make_panel)


# Text --------------------------------------------------------------------

title_text <- "Ten minutes of arc"

subtitle_text <- glue::glue(
  "A 10′ × 10′ square on the latitude/longitude grid is about ",
  "{round(equator$ns_km, 1)} km tall everywhere,<br>",
  "but it narrows toward the pole as the meridians close in. ",
  "Dashed outline: the same square at the equator."
)

caption_text <- glue::glue(
  "Data: Natural Earth  |  #30DayMapChallenge 2026 Day 07 – 10 minute map  |  ",
  "<img src='{bsky_logo_path}' width='10'/> @mel-likes-maps.bsky.social"
)


# Map ---------------------------------------------------------------------

p1 <- p_globe + wrap_plots(panels, ncol = 1) +
  plot_layout(widths = c(2.2, 1)) +
  plot_annotation(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    theme = theme(
      plot.background = element_rect(fill = bg_col, color = NA),
      plot.title = element_text(family = "src", face = "bold", size = 20,
                                color = ink_col, margin = margin(b = 4)),
      plot.subtitle = element_markdown(family = "src", size = 10, color = ink_col,
                                       lineheight = 1.3, margin = margin(b = 8)),
      plot.caption = element_markdown(family = "src", size = 7, color = "grey40"),
      plot.margin = margin(16, 16, 12, 16)
    )
  ) &
  theme(plot.background = element_rect(fill = bg_col, color = NA))


# plot(p1)

# With showtext dpi = 300, the plot pane shows text far too big. Judge the PNG.
ggsave(here::here("output", "day07_10minutes.png"), p1,
       width = 10, height = 8, dpi = 300, bg = bg_col)

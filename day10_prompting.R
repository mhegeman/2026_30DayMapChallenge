# Day 10 · Prompting only
# Every city that has hosted a WNBA franchise, 1997–2026, plus announced teams.
# Written and run entirely by Claude from chat prompts (the Day 10 rule).
# Team history and coordinates come from Claude's own knowledge, checked against
# the 2026 schedule from wehoop (ESPN). See map_notes/day10_prompting.md.

# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(ggtext)
library(showtext)

font_add_google("Barlow Condensed", "barlow")
font_add_google("Barlow", "barlowtext")
showtext_auto()
showtext_opts(dpi = 300)

bsky_logo_path <- here::here("img", "bsky_logo.png")

lcc <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +datum=WGS84 +units=m"

bg_col     <- "#F7F4EE"
land_col   <- "#E7E2D7"
line_col   <- "#FFFFFF"
ink_col    <- "#222222"
active_col <- "#E8590C"
gone_col   <- "#8A8A8A"
next_col   <- "#5B3FA0"


# Data --------------------------------------------------------------------

# One row per city and franchise stint. Status:
#   2026  = played the 2026 season there
#   Gone  = folded or moved away
#   Next  = expansion team announced, not yet playing
cities <- tribble(
  ~city,            ~lat,    ~lon,      ~status, ~label,
  "Brooklyn",       40.683,  -73.976,   "2026",  "Liberty 1997–",
  "Los Angeles",    34.043,  -118.267,  "2026",  "Sparks 1997–",
  "Phoenix",        33.446,  -112.071,  "2026",  "Mercury 1997–",
  "Washington",     38.857,  -76.995,   "2026",  "Mystics 1998–",
  "Minneapolis",    44.980,  -93.276,   "2026",  "Lynx 1999–",
  "Indianapolis",   39.764,  -86.155,   "2026",  "Fever 2000–",
  "Seattle",        47.622,  -122.354,  "2026",  "Storm 2000–",
  "Uncasville",     41.491,  -72.090,   "2026",  "Sun 2003–",
  "Chicago",        41.854,  -87.617,   "2026",  "Sky 2006–",
  "Atlanta",        33.648,  -84.449,   "2026",  "Dream 2008–",
  "Arlington",      32.731,  -97.114,   "2026",  "Wings 2016–",
  "Las Vegas",      36.090,  -115.177,  "2026",  "Aces 2018–",
  "San Francisco",  37.768,  -122.388,  "2026",  "Valkyries 2025–",
  "Toronto",        43.643,  -79.415,   "2026",  "Tempo 2026–",
  "Portland",       45.532,  -122.667,  "2026",  "Fire 2000–02,\nback 2026–",
  "Houston",        29.751,  -95.362,   "Gone",  "Comets 1997–2008",
  "Sacramento",     38.649,  -121.518,  "Gone",  "Monarchs 1997–2009",
  "Charlotte",      35.225,  -80.839,   "Gone",  "Sting 1997–2006",
  "Miami",          25.781,  -80.188,   "Gone",  "Sol 2000–02",
  "Orlando",        28.539,  -81.384,   "Gone",  "Miracle 1999–2002",
  "Salt Lake City", 40.768,  -111.901,  "Gone",  "Starzz 1997–2002",
  "San Antonio",    29.427,  -98.437,   "Gone",  "Silver Stars 2003–17",
  "Tulsa",          36.153,  -95.996,   "Gone",  "Shock 2010–15",
  "Cleveland",      41.497,  -81.688,   "Next",  "Rockers 1997–2003,\nnew team 2028",
  "Detroit",        42.341,  -83.055,   "Next",  "Shock 1998–2009,\nnew team 2029",
  "Philadelphia",   39.952,  -75.165,   "Next",  "New team 2030"
) |>
  mutate(status = factor(status, levels = c("2026", "Gone", "Next"))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(lcc)

# Relocations: same franchise, new city
moves <- tribble(
  ~from,            ~to,
  "Orlando",        "Uncasville",   # Miracle -> Sun, 2003
  "Salt Lake City", "San Antonio",  # Starzz -> Silver Stars, 2003
  "San Antonio",    "Las Vegas",    # Stars -> Aces, 2018
  "Detroit",        "Tulsa",        # Shock -> Tulsa Shock, 2010
  "Tulsa",          "Arlington"     # Tulsa Shock -> Wings, 2016
)

xy <- cities |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  select(city, x, y)

moves_xy <- moves |>
  left_join(xy, by = c("from" = "city")) |>
  left_join(xy, by = c("to" = "city"), suffix = c("", "end"))

# Basemap: US states and Canadian provinces, Mexico as one shape
states <- ne_states(country = c("United States of America", "Canada"),
                    returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii"))
mexico <- ne_countries(country = "Mexico", scale = 50, returnclass = "sf")

view_box <- st_bbox(c(xmin = -124.5, xmax = -67, ymin = 23.2, ymax = 49.5),
                    crs = 4326) |>
  st_as_sfc() |>
  st_segmentize(10000) |>
  st_transform(lcc) |>
  st_bbox()


# Labels ------------------------------------------------------------------

n_2026 <- sum(cities$status == "2026")

title_text <- "The WNBA keeps coming back"

subtitle_text <- glue::glue(
  "<span style='color:{active_col}'>**{n_2026} teams**</span> played the 2026 season. ",
  "<span style='color:{gone_col}'>**Six franchises folded**</span> and three moved, ",
  "five moves in all (arrows).<br>",
  "Portland got the Fire back in 2026. ",
  "<span style='color:{next_col}'>**Cleveland, Detroit and Philadelphia**</span> ",
  "have teams coming in 2028–2030."
)

caption_text <- glue::glue(
  "Data: Claude's knowledge of WNBA history, checked against the 2026 schedule (ESPN via wehoop) · ",
  "Basemap: Natural Earth<br>",
  "#30DayMapChallenge 2026 Day 10 – Prompting only. Made start to finish by Claude from chat prompts<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)


# Map ---------------------------------------------------------------------

p1 <- ggplot() +
  geom_sf(data = mexico, fill = land_col, color = line_col, linewidth = 0.3) +
  geom_sf(data = states, fill = land_col, color = line_col, linewidth = 0.3) +
  geom_curve(
    data = moves_xy,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = -0.25, linewidth = 0.45, color = gone_col,
    arrow = arrow(length = unit(1.8, "mm"), type = "closed")
  ) +
  geom_sf(
    data = cities,
    aes(color = status, fill = status, shape = status),
    size = 2.6, stroke = 0.9
  ) +
  geom_text_repel(
    data = cities,
    aes(label = label, geometry = geometry, color = status),
    stat = "sf_coordinates",
    family = "barlow", size = 3, lineheight = 0.85,
    bg.color = bg_col, bg.r = 0.12,
    min.segment.length = 0.2, segment.size = 0.2, box.padding = 0.35,
    max.overlaps = Inf, seed = 10
  ) +
  scale_color_manual(values = c("2026" = active_col, "Gone" = gone_col, "Next" = next_col),
                     guide = "none") +
  scale_fill_manual(values = c("2026" = active_col, "Gone" = bg_col, "Next" = bg_col),
                    guide = "none") +
  scale_shape_manual(values = c("2026" = 21, "Gone" = 21, "Next" = 23), guide = "none") +
  coord_sf(
    crs = lcc,
    xlim = c(view_box["xmin"], view_box["xmax"]),
    ylim = c(view_box["ymin"], view_box["ymax"]),
    expand = FALSE
  ) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
  theme_void(base_family = "barlowtext") +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    plot.title = element_text(
      family = "barlow", face = "bold", size = 28, color = ink_col,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 11, color = ink_col, lineheight = 1.3, margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      size = 7.5, color = "grey35", lineheight = 1.25, hjust = 0,
      margin = margin(t = 10)
    ),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(18, 22, 14, 22)
  )

# With showtext dpi = 300, the plot pane shows text far too big. Judge the PNG.
ggsave(here::here("output", "day10_prompting.png"), p1,
       width = 11, height = 7.6, dpi = 300, bg = bg_col)

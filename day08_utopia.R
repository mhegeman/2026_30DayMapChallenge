
# Setup -------------------------------------------------------------------

library(tidyverse)
library(arcgislayers)
library(sf)
library(ggtext)
library(tidycensus)    


bsky_logo_path <- here::here("img", "bsky_logo.png")

# Get Data ----------------------------------------------------------------

# NYC 311 Data for 2025

# pluto <- arc_open("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/ArcGIS/rest/services/MAPPLUTO/FeatureServer/0")

# Borough abbreviations: MN = Manhattan, BX = Bronx, BK = Brooklyn, QN = Queens, SI = Staten Island

# vacant lots in Brooklyn
# vacant_bk <- arc_select(
#   pluto, 
#   fields = c("BBL", "Borough", "LandUse", "LotArea", "OwnerType"), 
#   where = "LandUse = '11' And Borough = 'BK'"
# )
# 
# saveRDS(vacant_bk, here::here("data", "vacant_bk.rds"))


blocks <- arc_open("https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/Tracts_Blocks/MapServer/12")

bk_blocks <- arc_select(
  blocks,
  fields = c("GEOID", "POP100"),
  where  = "STATE = '36' AND COUNTY = '047'"
) |>
  sf::st_transform(2263) |>
  dplyr::filter(POP100 > 0)

nta_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson?$where=boroname='Brooklyn'"
nta <- st_read(nta_url, quiet = TRUE)

url <- paste0(
  "https://data.cityofnewyork.us/resource/enfh-gkve.geojson",
  "?$where=borough='B'&$limit=5000"
)

# Locations of NYC Parks in Brooklyn
bk_parks <- sf::st_read(URLencode(url), quiet = TRUE) |>
  sf::st_transform(2263) |> 
  filter(!typecategory %in% c("Strip", "Parkway", "Undeveloped", "Buildings/Institutions", "Lot", "Cemetery"))

vacant <- readRDS(here::here("data", "vacant_bk.rds"))




# Colors ------------------------------------------------------------------

bg_col    <- "#F5F1E8"  # warm paper
land_col  <- "#E2DCCF"  # neighborhoods
parks_col <- "#2F6B3F"  # deep park green
lots_col  <- "#9CCB6B"  # fresh green = could be a park



# Map ---------------------------------------------------------------------
title_text <- glue::glue("Utopia - More greenspace per person")

subtitle_text <- glue::glue(
  "Turning every <span style='color:{lots_col}'>vacant lot</span> green would add ",
  "1,335 acres to Brooklyn's <span style='color:{parks_col}'>parks</span>, about a third more"
)

caption_text <- glue::glue(
  "Data: NYC Open Data, MapPLUTO<br>",
  "#30DayMapChallenge 2026 Day 08 – Utopia<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)



p <- ggplot() +
  geom_sf(data = nta, fill = land_col, color = "white", linewidth = 0.2) +
  geom_sf(data = bk_parks, fill = parks_col, color = NA) +
  geom_sf(data = vacant, fill = lots_col, color = lots_col, linewidth = 0.35) +
  theme_void() +
  theme(
    plot.title = element_markdown(color = "grey20", face = "bold", size = 16),
    plot.subtitle = element_markdown(color = "grey40", face = "bold", size = 11, lineheight = 1.2),
    plot.caption = element_markdown(color = "grey55", size = 7.5, hjust = 0, margin = margin(t = 10)),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  ) +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)
p


ggsave(here::here("output", "day08_utopia.png"), p,
       width = 8, height = 9, dpi = 300, bg = bg_col)

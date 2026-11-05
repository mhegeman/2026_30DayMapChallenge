
# Setup -------------------------------------------------------------------

library(tidyverse)
library(httr2)
library(sf)
library(ggtext)


# Get Data ----------------------------------------------------------------

bsky_logo_path <- here::here("img", "bsky_logo.png")

manhattan_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson?$where=boroname='Manhattan'"
manhattan <- st_read(manhattan_url, quiet = TRUE)

# Clean Data --------------------------------------------------------------

# NYC 311 Data for 2025
complaints <- read_csv(here::here("data", "nyc311_2025.csv")) |> filter(borough == 'MANHATTAN') |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_join(manhattan |> select(ntaname, ntatype))


complaints_by_neighborhood <- complaints |> 
  st_drop_geometry() |> 
  filter(ntatype == 0) |> 
  group_by(ntaname, complaint_type) |> 
  summarise(num_complaints = n(), .groups = "drop") |> 
  slice_max(num_complaints, by = ntaname) |> 
  mutate(complaint_type = fct_infreq(str_to_title(complaint_type)))

n_distinct(complaints_by_neighborhood$complaint_type)


map_sf <- manhattan |> 
  select(ntaname) |> 
  left_join(complaints_by_neighborhood, by = "ntaname") |> 
  mutate(complaint_type = fct_na_value_to_level(complaint_type, level = "Park or non-residential"))


count(complaints_by_neighborhood, complaint_type, sort = TRUE)

# Make Map ----------------------------------------------------------------

okabe <- palette.colors(palette = "Okabe-Ito")[2:8]
# main_types <- setdiff(levels(complaints_by_neighborhood$complaint_type), "Other")
main_types <- levels(complaints_by_neighborhood$complaint_type)
fill_colors <- c(
  setNames(okabe[seq_along(main_types)], main_types),
  "Park or non-residential" = "grey95"
)

heat <- okabe[[1]]

p1 <- ggplot() +
  geom_sf(data = manhattan) +
  geom_sf(data = map_sf, aes(fill = complaint_type)) +
  scale_fill_manual(
    values   = fill_colors,
    na.value = "grey95",
    name     = NULL
  ) +
  coord_sf(xlim = c(-74.025, -73.905), ylim = c(40.685, 40.880), default_crs = 4326) +
  theme_void() +
  labs(
    title    = glue::glue(
      "<span style = 'color:{heat}'>Heat and hot water</span> topped the NYC 311 complaints<br>in nine Manhattan neighborhoods."
    ),
    subtitle = glue::glue("Parks and other non-residential areas were not included in the analysis."),
    caption  = glue::glue(
      "Data: NYC Open Data / NYC 311 2025<br>  ",
      "#30DayMapChallenge 2026 Day 03 \u2013 Polygons  <br>",
      "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
    )
  ) +
  theme(
    plot.title = element_markdown(color = "grey20", face = "bold", size = 16),
    plot.subtitle = element_markdown(color = "grey40", face = "bold", size = 11, lineheight = 1.2),
    plot.caption = element_markdown(color = "grey55", size = 7.5, hjust = 0, margin = margin(t = 10))
  )


plot(p1)



# Save --------------------------------------------------------------------
ggsave(here::here("output", "day03_polygons.png"), p1, width = 6, height = 9, dpi = 300, bg = "white")







# Setup -------------------------------------------------------------------

library(tidyverse)
library(httr2)
library(sf)
library(ggtext)


# Get Data ----------------------------------------------------------------

# NYC 311 Data for 2025

# url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
# req <- request(url) |> 
#   req_url_query(
#     `$select` = "unique_key, created_date, closed_date, agency_name, borough, complaint_type, descriptor, latitude, longitude", 
#     `$where` = "created_date >= '2025-01-01'
#                 AND created_date < '2026-01-01'
#                 AND latitude IS NOT NULL
#                 ",
#     `$limit` = 10000000
#   )
# 
# raw_data <- req |> 
#   req_perform() |> 
#   resp_body_string() |> 
#   read_csv(show_col_types = FALSE)
# 
# write_csv(raw_data, here::here("data", "nyc311_2025.csv"))


# streets_url <- "https://data.cityofnewyork.us/resource/inkn-q76z.geojson"
# manhattan_streets <- request(streets_url) |> 
#   req_url_query(
#     `$where` = "boroughcode = '1' AND rw_type = 1",
#     `$limit` = 50000
#   ) |> 
#   req_perform() |> 
#   resp_body_string() |> 
#   st_read(quiet = TRUE)
# 
# st_write(manhattan_streets, here::here("data", "manhattan_streets.gpkg"))


manhattan_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson?$where=boroname='Manhattan'"
manhattan <- st_read(manhattan_url)

# Clean Data --------------------------------------------------------------


complaints <- read_csv(here::here("data", "nyc311_2025.csv")) |> filter(borough == 'MANHATTAN') |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(crs = 2263)
streets <- st_read(here::here("data", "manhattan_streets.gpkg")) |> st_transform(crs = 2263)

# All complaints
nearest_street <- st_nearest_feature(complaints, streets)

# streets$physicalid[nearest_street]

complaints_sf <- complaints |> 
  mutate(physicalid = streets$physicalid[nearest_street])

complaint_count <- complaints_sf |> 
  st_drop_geometry() |> 
  count(physicalid)

streets_sf <- streets |> 
  left_join(complaint_count, by = "physicalid") |> 
  replace_na(list(n = 0)) |> 
  select(stname_label, rw_type, full_street_name, n, geom)

# Potholes

potholes <- complaints |> 
  filter(
    complaint_type == 'Street Condition'&
    descriptor == "Pothole"
  )

potholes_nearest_street <- st_nearest_feature(potholes, streets)

potholes_sf <- potholes |> 
  mutate(physicalid = streets$physicalid[potholes_nearest_street])

pothole_count <- potholes_sf |> 
  st_drop_geometry() |> 
  count(physicalid)

potholes_street_sf <- streets |> 
  left_join(pothole_count, by = "physicalid") |> 
  replace_na(list(n = 0)) |> 
  select(stname_label, rw_type, full_street_name, n, geom)



# Make Map ----------------------------------------------------------------

bsky_logo_path <- here::here("img", "bsky_logo.png")
line_color <- "navyblue"



# All complaints

p1 <- ggplot() +
  geom_sf(data = manhattan, fill = "grey95", color = "grey60") +
  geom_sf(
    data = streets_sf |> arrange(n) |> filter(n > 0),
    aes(color = n, linewidth = n)
    ) +
  # scale_color_stepsn(
  #   colors = complaint_pal,
  #   breaks = c()
  # )
  # geom_sf(data = complaints_sf |> head(1000), size = 1.00, alpha = 0.6, color = pink) +
  coord_sf(xlim = c(-74.025, -73.905), ylim = c(40.695, 40.880), default_crs = 4326) +
  theme_void() +
  labs(
    title    = glue::glue(
      "What streets have the most <br><span style='color:{line_color}'>311 Complaints</span> in Manhattan?"
    ),
    subtitle = glue::glue(" There were {sum(streets_sf$n)} complaints in 2025.<br> Each point is a complaint, not an ice cream truck."),
    caption  = glue::glue(
      "Data: NYC Open Data / NYC 311  <br>  ",
      "#30DayMapChallenge 2026 Day 01 \u2013 Points  <br>",
      "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
    )
  ) +
  theme(
    plot.title = element_markdown(color = "grey20", face = "bold", size = 16),
    plot.subtitle = element_markdown(color = "grey40", face = "bold", size = 11, lineheight = 1.2),
    plot.caption = element_markdown(color = "grey55", size = 7.5, hjust = 0, margin = margin(t = 10))
  )


plot(p1)


# Potholes
pothole_pal <- c(
  "#fec44f", 
  # "#fe9929", 
  "#ec7014", 
  # "#cc4c02", 
  "#662506"
  )



p2 <- ggplot() +
  geom_sf(data = manhattan, fill = "grey95", color = "grey60") +
  geom_sf(data = streets_sf, color = "grey80") +
  geom_sf(
    data = potholes_street_sf |> arrange(n) |> filter(n > 0),
    aes(color = n, linewidth = n)
  ) +
  scale_color_stepsn(
    colors = pothole_pal,
    breaks = c(2,4,8,16),
    limits = c(1, 24),
    transform = "log2",
    name = "Potholes per\nstreet segment"
  ) +
  scale_linewidth(range = c(0.3, 1.4), guide = "none") +  
  coord_sf(xlim  = c(-74.025, -73.905), ylim = c(40.695, 40.880), default_crs = 4326) +
  theme_void() +
  labs(
    title = "Which streets deal<br>with potholes the most?",
    subtitle = glue::glue("There were {format(sum(potholes_street_sf$n), big.mark = ',')} complaints in 2025."),
    caption  = glue::glue(
      "Data: NYC Open Data / NYC 311  <br>  ",
      "#30DayMapChallenge 2026 Day 02 \u2013 Lines  <br>",
      "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
    )
  )  +
  theme(
    plot.title = element_markdown(color = "grey20", face = "bold", size = 16),
    plot.subtitle = element_markdown(color = "grey40", face = "bold", size = 11, lineheight = 1.2),
    plot.caption = element_markdown(color = "grey55", size = 7.5, hjust = 0, margin = margin(t = 10))
  )
  


plot(p2)

# Save --------------------------------------------------------------------
ggsave(here::here("output", "day02_lines.png"), p2, width = 6, height = 9, dpi = 300, bg = "white")






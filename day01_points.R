
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

manhattan_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson?$where=boroname='Manhattan'"
manhattan <- st_read(manhattan_url)

# Clean Data --------------------------------------------------------------

complaints <- read_csv(here::here("data", "nyc311_2025.csv")) |> filter(borough == 'MANHATTAN')

# complaints_by_type <- complaints |> 
#   group_by(agency_name, complaint_type, descriptor) |> 
#   summarise(num_complaints = n())

ice_cream <- complaints |> filter(str_detect(descriptor, "Ice Cream"))


# Make Map ----------------------------------------------------------------

bsky_logo_path <- here::here("img", "bsky_logo.png")
pink <- "deeppink"
ice_cream_sf <- st_as_sf(ice_cream, coords = c("longitude", "latitude"), crs = 4326)

p1 <- ggplot() +
  geom_sf(data = manhattan, fill = "grey95", color = "grey60") +
  geom_sf(data = ice_cream_sf, size = 1.00, alpha = 0.6, color = pink) +
  coord_sf(xlim = c(-74.025, -73.905), ylim = c(40.695, 40.880)) +
  theme_void() +
  labs(
    title    = glue::glue(
      "<span style='color:{pink}'>Ice Cream Truck</span> Noise Complaints<br> in Manhattan"
    ),
    subtitle = glue::glue(" There were {nrow(ice_cream_sf)} complaints in 2025.<br> Each point is a complaint, not an ice cream truck."),
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

# Save --------------------------------------------------------------------
ggsave(here::here("output", "day01_points.png"), p1, width = 6, height = 9, dpi = 300, bg = "white")


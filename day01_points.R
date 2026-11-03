
# Setup -------------------------------------------------------------------

library(tidyverse)
library(httr2)
library(sf)


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

# Clean Data --------------------------------------------------------------

complaints <- read_csv(here::here("data", "nyc311_2025.csv")) |> filter(borough == 'MANHATTAN')

complaints_by_type <- complaints |> 
  group_by(agency_name, complaint_type, descriptor) |> 
  summarise(num_complaints = n())

ice_cream <- complaints |> filter(str_detect(descriptor, "Ice Cream"))


# Make Map ----------------------------------------------------------------

ice_cream_sf <- st_as_sf(ice_cream, coords = c("longitude", "latitude"), crs = 4326)

p1 <- ggplot() +
  geom_sf(data = ice_cream_sf, size = 0.8, alpha = 0.6, color = "deeppink")


plot(p1)

# Save --------------------------------------------------------------------



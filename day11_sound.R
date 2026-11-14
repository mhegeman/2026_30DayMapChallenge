
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
# library(httr2)
library(ggtext)

# Get Data ----------------------------------------------------------------

bsky_logo_path <- here::here("img", "bsky_logo.png")

nyc_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson"
nyc <- st_read(nyc_url, quiet = TRUE) |> st_transform(crs = 4326)

# Clean Data --------------------------------------------------------------

# NYC 311 Data for 2025
complaints <- read_csv(here::here("data", "nyc311_2025.csv")) |> 
  filter(complaint_type == "Noise - Helicopter") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_join(nyc |> select(ntaname, ntatype))

complaints_by_neighborhood <- complaints |>
  st_drop_geometry() |>
  filter(ntatype == 0) |>
  group_by(ntaname, complaint_type) |>
  summarise(num_complaints = n(), .groups = "drop")



map_centroids <- nyc |> 
  select(ntaname) |> 
  left_join(complaints_by_neighborhood, by = "ntaname") |> 
  st_centroid() |> 
  filter(!is.na(num_complaints))

top <- map_centroids |>
  st_drop_geometry() |>
  slice_max(num_complaints, n = 1)

top_n    <- scales::label_comma()(top$num_complaints)   
top_name <- top$ntaname                                 


# Make Map ----------------------------------------------------------------

loud_threshold <- 500
highlight_color <- "#C0392B"

title_text <- glue::glue(
  "Where NYC hears helicopters"
)
subtitle_text <-  glue::glue(
  "The {top_name} neighborhood<br>",
  "logged the most with <span style = 'color:{highlight_color}'>**{top_n}**</span> complaints in 2025.<br>",
  "Rings mark neighborhoods with {loud_threshold}+ complaints."
  )
caption_text <-  glue::glue(
  "Data: NYC Open Data / NYC 311 2025<br>  ",
  "#30DayMapChallenge 2026 Day 11 \u2013 Sound<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)


loud  <- map_centroids |> 
  filter(num_complaints >= loud_threshold) |> 
  mutate(label = case_when(
    ntaname == "Upper West Side-Lincoln Square" ~ "Upper West Side",
    ntaname == "Upper West Side (Central)" ~ NA,   # shares the label above
    ntaname == "Howard Beach-Lindenwood" ~ "Howard Beach",
    ntaname == "Murray Hill-Kips Bay" ~ "Murray Hill",
    ntaname == "Carroll Gardens-Cobble Hill-Gowanus-Red Hook" ~ "Carroll Gardens\n& Red Hook",
    ntaname == "Pomonok-Electchester-Hillcrest" ~ "Pomonok",
    .default = ntaname                                           
  ))

quiet <- map_centroids |> filter(num_complaints <  loud_threshold)


p1 <- ggplot() +
  geom_sf(data = nyc, fill = "grey92", color = "white", linewidth = 0.2) +
  geom_sf(data = loud, aes(size = num_complaints),
          shape = 21, fill = NA, color = highlight_color, stroke = 1.4, alpha = 0.5) +
  geom_sf(data = loud, aes(size = num_complaints * 0.44),
          shape = 21, fill = NA, color = highlight_color, stroke = 1.0, alpha = 0.6,
          show.legend = FALSE) +
  geom_sf(data = loud, aes(size = num_complaints * 0.11),
          shape = 21, fill = NA, color = highlight_color, stroke = 0.8, alpha = 0.9,
          show.legend = FALSE) +
  geom_sf(data = quiet, size = 0.5, shape = 16, color = highlight_color, alpha = 0.4) +
  scale_size_area(max_size = 14,
                  breaks = c(500, 2000, 6000),
                  labels = scales::label_comma()) +
  ggrepel::geom_text_repel(
    data = loud,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    nudge_x = -0.02, 
    size = 3, color = "grey20", fontface = "bold",
    bg.color = "white", bg.r = 0.15,
    box.padding = 1, min.segment.length = 0,
    segment.color = "grey50", segment.size = 0.3,
    na.rm = TRUE
  ) +
  coord_sf(default_crs = 4326) +
  theme_void() +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text, 
    size = "Complaints, 2025"
  ) +
  guides(size = guide_legend(override.aes = list(stroke = 0.8, alpha = 0.8))) +
  theme(
    plot.title = element_markdown(color = "grey20", face = "bold", size = 16),
    plot.subtitle = element_markdown(color = "grey40", size = 11, lineheight = 1.2),
    plot.caption = element_markdown(color = "grey55", size = 7.5, hjust = 0, margin = margin(t = 10)),
    legend.title = element_text(color = "grey30", size = 9, face = "bold"),
    legend.text  = element_text(color = "grey40", size = 8),
    legend.key.spacing.y = unit(4, "pt"),
    plot.margin = margin(20, 20, 20, 20)
  )

plot(p1)



# Save --------------------------------------------------------------------
ggsave(here::here("output", "day11_sound.png"), p1,
       width = 9, height = 8, dpi = 300, bg = "white")



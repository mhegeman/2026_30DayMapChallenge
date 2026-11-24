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



# Data --------------------------------------------------------------------

greenland <- s2::s2_data_countries("Greenland") |>
  sf::st_as_sfc()


st_crs(greenland)

greenland_webMercator <- st_transform(greenland, crs = 3857)
greenland_equalEarth <- st_transform(greenland, crs = 8857)
greenland_local <- st_transform(greenland, crs = "+proj=laea +lat_0=72 +lon_0=-42 +datum=WGS84")

st_area(st_transform(greenland, 3857))

shapes <- list(greenland_webMercator, greenland_equalEarth, greenland_local)

half_w <- max(sapply(shapes, \(x) diff(st_bbox(x)[c("xmin", "xmax")]))) / 2
half_h <- max(sapply(shapes, \(x) diff(st_bbox(x)[c("ymin", "ymax")]))) / 2

# Area each projection draws Greenland at, in million km². On a projected CRS,
# st_area() measures flat map area, so Mercator's inflation shows up here.
area_label <- function(shape) {
  million_km2 <- as.numeric(st_area(shape)) / 1e12
  paste0("drawn as ", round(million_km2, 1), " million km²")
}

plot_panel <- function(shape, title, preserves, note) {
  bb <- st_bbox(shape)
  cx <- mean(bb[c("xmin", "xmax")])
  cy <- mean(bb[c("ymin", "ymax")])
  
  ggplot() +
    geom_sf(data = shape, fill = "#0B6E4F") +
    coord_sf(
      xlim = cx + c(-half_w *1.08, half_w*1.08),
      ylim = cy + c(-half_h*1.08, half_h*1.08),
      expand = FALSE
    ) +
    labs(
      title = title,
      subtitle = paste0(preserves, " · ", area_label(shape), "\n", note)
    ) +
    theme_void() +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey30", lineheight = 1.2,
                                   margin = margin(t = 2, b = 6)),
      plot.margin = margin(r = 14)   # gap so panel subtitles don't touch
    )
}




# Titles and caption ------------------------------------------------------

# True (geodesic) area vs. the flat area Mercator draws
true_area <- as.numeric(st_area(greenland))
merc_ratio <- as.numeric(st_area(greenland_webMercator)) / true_area

title_text <- glue::glue(
  "Mercator draws Greenland {round(merc_ratio)} times too big"
)

subtitle_text <- glue::glue(
  "One Greenland in three projections, all at one shared map scale. ",
  "The equal-area projections<br>",
  "preserve its true size, about {round(true_area / 1e12, 1)} million km²."
)

caption_text <- glue::glue(
  "Data: Natural Earth via the s2 package · ",
  "#30DayMapChallenge 2026 Day 22 – Projections · ",
  "<img src='{bsky_logo_path}' width='10'/> @mel-likes-maps.bsky.social"
)
 

# Map ---------------------------------------------------------------------

p1 <- plot_panel(
  greenland_webMercator, "Web Mercator", "Preserves angles",
  "Local shape true, size grows toward poles"
) +
  plot_panel(
    greenland_equalEarth, "Equal Earth", "Preserves area",
    "World map, sheared at high latitudes"
  ) +
  plot_panel(
    greenland_local, "Lambert azimuthal equal-area", "Preserves area",
    "Centered on Greenland, shape nearly true"
  ) +
  plot_layout(nrow = 1) +
  plot_annotation(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    theme = theme(
      plot.title = element_text(family = "src", face = "bold", size = 20,
                                margin = margin(b = 4)),
      plot.subtitle = element_markdown(family = "src", size = 10,
                                       lineheight = 1.3, margin = margin(b = 12)),
      plot.caption = element_markdown(family = "src", size = 7, color = "grey40",
                                      margin = margin(t = 10)),
      plot.margin = margin(16, 16, 12, 16)
    )
  ) &
  theme(text = element_text(family = "src"))

ggsave(here::here("output", "day22_projections.png"), p1,
       width = 12, height = 5.5, dpi = 300)

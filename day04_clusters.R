
# Setup -------------------------------------------------------------------

library(wehoop)
library(dbscan)
library(sportyR)
library(tidyverse)
library(ggtext)
library(showtext)

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)

valkyries <- 129689   # ESPN team_id for the Golden State Valkyries

hoop_x   <- 41.75   # hoop is 5.25 ft in from the baseline at x = 47
arc_dist <- 22.15   # WNBA three-point distance at the top of the key (ft)

bsky_logo_path <- here::here("img", "bsky_logo.png")


# Get data ----------------------------------------------------------------

# Full season (regular + playoffs). The full season is only used to measure
# the coordinate stretch below; the map uses playoff games only.
shots_season <- load_wnba_pbp(seasons = 2026) |>
  filter(
    team_id == valkyries,
    shooting_play,
    !str_detect(type_text, "Free Throw"),
    abs(coordinate_x) <= 50,
    abs(coordinate_y) <= 25
  ) |>
  # 1. Fold every shot onto the right-hand basket (rotate 180 degrees)
  # 2. Drop backcourt heaves, which the fold sends to the wrong basket
  mutate(
    flip      = coordinate_x < 0,
    x         = if_else(flip, -coordinate_x, coordinate_x),
    y         = if_else(flip, -coordinate_y, coordinate_y),
    text_dist = as.numeric(str_extract(text, "\\d+(?=-foot)")),
    three     = str_detect(text, "three point")
  ) |>
  filter(is.na(text_dist) | text_dist < 40)


# Fix the coordinate stretch ----------------------------------------------

# ESPN's x is stretched along the court. Measure the stretch from the
# two/three boundary on straight-on shots (full season, for a steadier
# estimate), then pull x back toward the hoop.
boundary <- shots_season |>
  mutate(dx = hoop_x - x) |>
  filter(abs(y) < 4, dx > 15, dx < 35) |>
  summarise(
    two_q95   = quantile(dx[!three], 0.95),
    three_q05 = quantile(dx[three], 0.05)
  ) |>
  mutate(boundary = (two_q95 + three_q05) / 2)

boundary   # check these numbers look sensible

s <- boundary$boundary / arc_dist

shots_season <- shots_season |>
  mutate(x = hoop_x - (hoop_x - x) / s)


# Playoffs only -----------------------------------------------------------

shots <- shots_season |>
  filter(season_type == 3)   # 2 = regular season, 3 = playoffs

shots |> distinct(game_id, game_date) |> arrange(game_date)   # check the games
nrow(shots)                                                    # how many shots


# Checks ------------------------------------------------------------------

# Twos and threes should split at the painted arc
print(
  geom_basketball("wnba", display_range = "offense") +
    geom_point(data = shots, aes(x, y, color = three), alpha = 0.4)
)


# Clusters ----------------------------------------------------------------

# Weighted DBSCAN: makes count 1, misses 0, so a spot is dense only when
# enough made shots are packed together. Tune these two for the playoffs.
eps_made     <- 2.5
min_pts_made <- 4

xy <- as.matrix(shots[, c("x", "y")])
w  <- as.numeric(shots$scoring_play)   # make = 1, miss = 0

# kNN plot on made shots only, to find the elbow for eps_made
kNNdistplot(xy[w == 1, ], k = min_pts_made)
abline(h = eps_made, lty = 2)

db_made <- dbscan(xy, eps = eps_made, minPts = min_pts_made, weights = w)
db_made

shots <- shots |>
  mutate(cluster_made = factor(db_made$cluster))


# Cluster names -----------------------------------------------------------

# DBSCAN numbers clusters arbitrarily. Check db_made and the map, then fill
# these in. Any cluster not listed becomes "Other"; 0 is always "Noise".
names_made <- c("1" = "Paint", "2" = "Arc", "3" = "Arc",
                "4" = "Arc", "5" = "Arc", "6" = "Arc")

name_clusters <- function(cl, lookup) {
  cl <- as.character(cl)
  case_when(
    cl == "0"             ~ "Noise",
    cl %in% names(lookup) ~ unname(lookup[cl]),
    TRUE                  ~ "Other"
  )
}

tiles <- shots |>
  mutate(group = name_clusters(cluster_made, names_made)) |>
  count(x, y, group)


# Colors ------------------------------------------------------------------

bg_col   <- "#F7F5F0"   # off-white court and background
line_col <- "grey70"    # court lines
text_col <- "grey20"

group_cols <- c(
  "Paint"       = "#E69F00",
  "Arc"         = "#0072B2",
  "Upper elbow" = "#CC79A7",  # swap for the Valkyries' official violet
  "Other"       = "grey88",
  "Noise"       = "grey88"
)

# Court colors. Feature names vary by league, so list them with
# sportyR::cani_color_league_features("wnba") and adjust if one errors.
court_cols <- list(
  offensive_half_court      = bg_col,
  defensive_half_court      = bg_col,
  court_apron               = bg_col,
  two_point_range           = bg_col,
  painted_area              = bg_col,
  center_circle_fill        = bg_col,
  free_throw_circle_fill    = bg_col,
  sideline                  = line_col,
  endline                   = line_col,
  division_line             = line_col,
  three_point_line          = line_col,
  lane_boundary             = line_col,
  center_circle_outline     = line_col,
  free_throw_circle_outline = line_col
)


# Text --------------------------------------------------------------------

title_text <- glue::glue("Clusters of successful shots by the Valkyries<br>during the 2026 playoffs.")

subtitle_text <- glue::glue(
  "{nrow(shots)} shots were taken during the postseason. Points were clustered at the <span style='color:{group_cols['Paint']}'>rim</span> and beyond the <span style='color:{group_cols['Arc']}'>3-point line</span>."
)
  
caption_text <- glue::glue(
  "Data: ESPN via wehoop<br>",
  "#30DayMapChallenge 2026 Day 04 – Clusters<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)

centers <- shots |>
  filter(cluster_made != "0") |>
  group_by(cluster_made) |>
  summarise(x = median(x), y = median(y), n = n())

geom_basketball("wnba", display_range = "offense") +
  geom_point(data = filter(shots, cluster_made != "0"),
             aes(x, y, color = cluster_made), alpha = 0.6) +
  geom_label(data = centers, aes(x, y, label = cluster_made))


# Map ---------------------------------------------------------------------

p <- geom_basketball(
  "wnba",
  display_range = "offense",
  color_updates = court_cols
) +
  geom_tile(
    data = tiles,
    aes(x, y, fill = group, alpha = n),
    width = 1 / s, height = 1
  ) +
  scale_fill_manual(values = group_cols) +
  scale_alpha(range = c(0.35, 1), trans = "log10") +
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  theme(
    legend.position  = "none",
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    plot.title = element_markdown(
      family = "lato", face = "bold", size = 20, color = text_col,
      margin = margin(t = 10, b = 6)
    ),
    plot.subtitle = element_markdown(
      family = "lato", size = 11, color = text_col, lineheight = 1.2,
      margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      family = "lato", size = 8, color = "grey45", lineheight = 1.2
    ),
    plot.margin = margin(10, 15, 15, 15)
  )

p

ggsave(here::here("output", "day04_clusters.png"), p,
       width = 8, height = 9, dpi = 300, bg = bg_col)

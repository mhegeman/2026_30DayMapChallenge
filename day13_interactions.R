# Setup -------------------------------------------------------------------

library(wehoop)
library(sportyR)
library(tidyverse)
library(ggtext)
library(showtext)
library(ggfx)

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
  mutate(
    with_assist = str_detect(text, "assist")
  ) |> 
  select(
    game_play_number, season_type, game_id, game_date, type_id, type_text, text, scoring_play, score_value, athlete_name_1, athlete_name_2, with_assist, coordinate_x, coordinate_y
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
  filter(season_type == 3 & scoring_play == TRUE)    # 2 = regular season, 3 = playoffs
  

# successful shots   
shots |> distinct(game_id, game_date) |> arrange(game_date)   # check the games
nrow(shots)                                                    # how many shots

# Checks ------------------------------------------------------------------

# Twos and threes should split at the painted arc
print(
  geom_basketball("wnba", display_range = "offense") +
    geom_point(data = shots, aes(x, y, color = three), alpha = 0.4)
)



# Colors ------------------------------------------------------------------


offical_colors <- espn_wnba_teams() |>
  filter(team_id == valkyries) |>
  select(display_name, color, alternate_color)

primary_color <- offical_colors |> pull(color) |> str_pad(7, side = c("left"), pad = "#")
secondary_color <- offical_colors |> pull(alternate_color) |> str_pad(7, side = "left", pad = "#")


bg_col   <- "#15121C"   
line_col <- "grey30"    # court lines
text_col <- "grey90"


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
  free_throw_circle_outline = line_col,
  free_throw_circle_dash    = line_col,
  substitution_line         = bg_col, 
  sideline = line_col,
  inbounding_line = line_col, 
  backboard = line_col, 
  baseline_lower_defensive_box = line_col,
  lane_lower_defensive_box = line_col, 
  court_apron = line_col, 
  basket_ring = line_col, 
  lane_space_mark = line_col,
  net = line_col, 
  division_line = line_col, 
  lane_boundary = line_col, 
  restricted_arc = line_col
)


# Text --------------------------------------------------------------------

num_assists <- shots |> filter(with_assist)

title_text <- glue::glue("Most Valkyries baskets started with a pass")

subtitle_text <- glue::glue(
  "Valkyries made {nrow(shots)} shots in the 2026 playoffs and <span style='color:{primary_color}'>{nrow(num_assists)} had assists</span>."
)

caption_text <- glue::glue(
  "Data: ESPN via wehoop<br>",
  "#30DayMapChallenge 2026 Day 13 – Interactions<br>",
  "<img src='{bsky_logo_path}' width='12'/> @mel-likes-maps.bsky.social"
)



# geom_basketball("wnba", display_range = "offense") +
#   geom_point(data = shots,
#              aes(x, y, color = with_assist), alpha = 0.6) 

# Map ---------------------------------------------------------------------

p <- geom_basketball(
  "wnba",
  display_range = "offense",
  color_updates = court_cols
    ) +
  geom_point(
    data = filter(shots, !with_assist), 
    aes(x, y),
    color = "grey35", 
    size = 1.5,
    position = position_jitter(width = 0.4/s, height = 0.4, seed = 13)
    ) +
  with_outer_glow(
    geom_point(
      data = filter(shots, with_assist), 
      aes(x, y),
      color = primary_color, 
      size = 1.2, 
      alpha = 0.7,
      position = position_jitter(width = 0.4/s, height = 0.4, seed = 13)
      ),
    colour = primary_color, sigma = 7, expand = 3
  ) +
  # scale_color_manual(values = c(`TRUE` = group_cols[['Arc']], `FALSE` = "grey70")) +
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
      family = "lato", size = 14, color = text_col, lineheight = 1.2,
      margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      family = "lato", size = 8, color = "grey60", lineheight = 1.2
    ),
    plot.margin = margin(10, 15, 15, 15)
  )

p

ggsave(here::here("output", "day13_interactions.png"), p,
       width = 8, height = 9, dpi = 300, bg = bg_col)


# Some additional stats ---------------------------------------------------

# what % of threes were assists vs 2-pointers? 

x <- shots |> 
  group_by(three, with_assist) |> 
  summarise(n = n())



#


## Library ----

library(ggplot2)
library(plotly)
library(shiny)
library(bslib)
library(treemapify)

## Cleaning ----

luka_trade_date <- as.Date("2025-02-01")

zone_order <- c(
  "Restricted Area",
  "In The Paint (Non-RA)",
  "Mid-Range",
  "Left Corner 3",
  "Right Corner 3",
  "Above the Break 3"
)

lakers_shots <- readRDS("data/cleaned/lakers_shots.rds") |>
  mutate(
    game_date = as.Date(game_date),
    era = factor(
      if_else(game_date < luka_trade_date, "AD Era", "Luka Era"),
      levels = c("AD Era", "Luka Era")
    )
  ) |>
  filter(shot_zone_basic %in% zone_order) |>
  mutate(shot_zone_basic = factor(shot_zone_basic, levels = zone_order))

shot_baseline <- lakers_shots |>
  group_by(shot_zone_basic, action_type) |>
  summarise(xefg_baseline = sum(shot_made_flag * shot_value) / (2 * n()),
            .groups = "drop")

lakers_shots <- lakers_shots |>
  left_join(shot_baseline, by = c("shot_zone_basic", "action_type"))

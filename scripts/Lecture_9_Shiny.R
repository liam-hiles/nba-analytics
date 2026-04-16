# Lecture 9 ----

## Packages ----

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

## Data ----

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


## Static Plot ----

rui_by_zone <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  count(era, shot_zone_basic) |>
  group_by(era) |>
  mutate(share = n / sum(n)) |>
  ungroup()

rui_by_zone


lakers_colours <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")

static_plot_rui <- ggplot(rui_by_zone,
       aes(x = shot_zone_basic, y = share, fill = era)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = lakers_colours) +
  labs(
    title    = "Rui Hachimura: shot distribution by zone",
    subtitle = "Share of attempts from each zone, AD Era vs Luka Era",
    x = NULL, y = "Share of attempts", fill = NULL,
    caption  = "Data: NBA Stats. AD Era = before 1 Feb 2025, Luka Era = from 1 Feb 2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank()
  )


## Plotly ----

rui_plot <- ggplot(rui_by_zone,
                   aes(x = shot_zone_basic, y = share, fill = era,
                       text = paste0(
                         era, "<br>",
                         shot_zone_basic, "<br>",
                         "Attempts: ", n, "<br>",
                         "Share: ", round(share * 100, 1), "%"
                       ))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = lakers_colours) +
  labs(fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggplotly(rui_plot, tooltip = "text")


## Clean for shiny ----

shot_zones <- lakers_shots |>
  count(player_name, era, shot_zone_basic) |>
  group_by(player_name, era) |>
  mutate(share = n / sum(n)) |>
  ungroup()

shot_zones

saveRDS(shot_zones, "app/shot_zones.rds")

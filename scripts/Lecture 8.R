# Week 8 Lecture Run-Through -----

## Initial Set-up ----

library(tidyverse)
library(hoopR)

## SportsDataVerse ----

### Inspect the function ----
?load_nba_team_box
?load_nba_player_box

### Pull Data ----
nba_team_box <- load_nba_team_box(seasons = 2024)

### Inspect the Data ----
dim(nba_team_box)
names(nba_team_box)
glimpse(nba_team_box)
head(nba_team_box)

### Prepare the data, cleaning ----
nba_regular <- nba_team_box |>
  filter(season_type == 2)

nba_team_stats <- nba_regular |>
  group_by(team_short_display_name) |>
  summarise(
    games = n(),
    wins = sum(team_winner == TRUE, na.rm = TRUE),
    losses = sum(team_winner == FALSE, na.rm = TRUE),
    avg_pts = round(mean(team_score, na.rm = TRUE), 1),
    avg_fg_pct = round(mean(field_goals_made / field_goals_attempted * 100, na.rm = TRUE), 1),
    avg_3pt_made = round(mean(three_point_field_goals_made, na.rm = TRUE), 1),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 1),
    avg_assists = round(mean(assists, na.rm = TRUE), 1),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 1),
    avg_steals = round(mean(steals, na.rm = TRUE), 1),
    avg_blocks = round(mean(blocks, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(
    win_pct = round(wins / games * 100, 1)
  ) |>
  arrange(desc(win_pct))

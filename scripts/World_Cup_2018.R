## Packages ----

library(StatsBombR)
library(tidyverse)
library(ggsoccer)
library(ggplot2)
library(shinylive)

## Data Wrangling ----

comps <- FreeCompetitions() #%>% 
  #glimpse()

wc <- comps[comps$competition_name == 'FIFA World Cup' & comps$season_name == '2018', ]

matches <- FreeMatches(Competitions = wc)

events <- free_allevents(MatchesDF = matches)
events <- allclean(events)
#unique(events$type.name)
#glimpse(events)

## Data Cleaning + Prep ----

pass_shot_uc <- events %>% 
  filter(type.name == 'Pass'| type.name == 'Shot',
         team.name == 'France') %>% 
  select(match_id, team.name, player.name, position.name, location.x, location.y,
         type.name, under_pressure, TimeInPoss, play_pattern.name, 
         starts_with('pass'), starts_with('shot'))

#glimpse(pass_shot_uc)


attacking_influence <- pass_shot_uc %>% 
  filter(player.name == 'Antoine Griezmann')

#glimpse(attacking_influence)

saveRDS(attacking_influence, 'data/cleaned/attacking_influence.rds')

shots <- attacking_influence %>% 
  filter(type.name == 'Shot')

passes <- attacking_influence %>% 
  mutate(pass.outcome.name = case_when(is.na(pass.outcome.name) ~ 'Complete',
                                       TRUE ~ 'Incomplete')) %>% 
  filter(type.name == 'Pass',
         pass.outcome.name == 'Complete') %>% 
  mutate(
    start_dist_to_goal = sqrt((120 - location.x)^2 + (40 - location.y)^2),
    end_dist_to_goal   = sqrt((120 - pass.end_location.x)^2  + (40 - pass.end_location.y)^2),
    progressive_pass = case_when(
      location.x < 40 & pass.end_location.x < 40 ~ FALSE,
      end_dist_to_goal < start_dist_to_goal * 0.75 ~ TRUE,
      TRUE ~ FALSE
    )
  )

## Data Vis ----

map_shot <- ggplot(data = shots) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = 'white', fill = 'darkgreen') +
  theme_pitch() +
  coord_flip(xlim = c(60, 120), ylim = c(0, 80)) + 
  geom_point(aes(x = location.x,
                 y = location.y,
                 colour = shot.outcome.name,
                 size = 1))

map_pass <- ggplot(data = passes) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = 'white', fill = 'darkgreen') +
  theme_pitch() + 
  geom_segment(aes(
    x = location.x, y = location.y,
    xend = pass.end_location.x, yend = pass.end_location.y,
    colour = pass.height.name,
    linetype = progressive_pass),
  arrow = arrow(length = unit(0.3, 'cm'), type = 'closed'),
  alpha = 0.8
  ) +
  scale_linetype_manual(values = c('FALSE' = 'dashed', 'TRUE' = 'solid')) +
  scale_colour_manual(values = c('Ground Pass' = '#FF6B00', 'High Pass' = '#00BFFF', 'Low Pass' = '#FFD700'))


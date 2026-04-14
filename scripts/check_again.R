## packages ----

library(StatsBombR)
library(tidyverse)


## data wrangling ----

comps <- FreeCompetitions() %>% 
  glimpse()

wc <- comps[comps$competition_name == "FIFA World Cup" & comps$season_name == "2018", ]

matches <- FreeMatches(Competitions = wc)

events <- free_allevents(MatchesDF = matches)
events <- allclean(events)
unique(events$type.name)


## data cleaning + prep ----

passes_uc <- events %>% 
  filter(type.name == 'Pass') %>% 
  select(match_id, possession_team.name, player.name, position.name, 
         pass.length, pass.angle, pass.end_location.x, pass.end_location.y,
         play_pattern.name, under_pressure, TimeInPoss, pass.cross, pass.switch, pass.shot_assist, 
         pass.aerial_won, pass.goal_assist, pass.height.name, pass.type.name, pass.body_part.name, 
         pass.outcome.name)

glimpse(passes_uc)






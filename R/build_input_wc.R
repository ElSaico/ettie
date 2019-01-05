library(dplyr)
library(mlr)

build.input.wc <- function(matches, metrics) {
  matches <- filter(matches, competition.competition_name == "FIFA World Cup")
  home.teams <- matches %>%
    inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name")) %>%
    inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    mutate(result = factor(case_when(home_score > away_score ~ "win", home_score < away_score ~ "loss", TRUE ~ "draw")))
  away.teams <- matches %>%
    inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name")) %>%
    inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    mutate(result = factor(case_when(home_score < away_score ~ "win", home_score > away_score ~ "loss", TRUE ~ "draw")))
  rbind(home.teams, away.teams) %>% select(result, ends_with(".team"), ends_with(".adv"))
}

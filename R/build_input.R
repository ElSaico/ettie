library(mlr)

build.input <- function(matches, metrics, by.team = FALSE) {
  if (!by.team) {
    matches %>%
      inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name")) %>%
      inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name"), suffix = c(".home", ".away")) %>%
      mutate(result = factor(case_when(home_score > away_score ~ "home", home_score < away_score ~ "away", TRUE ~ "draw"))) %>%
      select(result, ends_with(".home"), ends_with(".away"))
  } else {
    home.teams <- matches %>%
      inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name")) %>%
      mutate(home = as.numeric(competition.competition_name != "FIFA World Cup"), result = factor(case_when(home_score > away_score ~ "win", home_score < away_score ~ "loss", TRUE ~ "draw"))) %>%
      select(result, home, avg.recovery.time, avg.player.possession, avg.team.possession, shots, expulsions, passes, successful.passes, corners, fouls)
    away.teams <- matches %>%
      inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name")) %>%
      mutate(home = 0, result = factor(case_when(home_score < away_score ~ "win", home_score > away_score ~ "loss", TRUE ~ "draw"))) %>%
      select(result, home, avg.recovery.time, avg.player.possession, avg.team.possession, shots, expulsions, passes, successful.passes, corners, fouls)
    rbind(home.teams, away.teams)
  }
}

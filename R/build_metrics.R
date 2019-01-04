library(dplyr)

build.metrics <- function(events) {
  metrics <- events %>%
    group_by(match_id, team.name) %>%
    summarise(
      shots = sum(type.name == "Shot"),
      expulsions = sum(foul_committed.card.name %in% c("Red Card", "Second Yellow")) + sum(bad_behaviour.card.name %in% c("Red Card", "Second Yellow")),
      passes = sum(type.name == "Pass"),
      successful.passes = sum(type.name == "Ball Receipt*"),
      corners = sum(pass.type.name == "Corner", na.rm = TRUE),
      fouls = sum(type.name == "Foul Committed"),
      avg.team.possession = mean(PossessionRate, na.rm = TRUE)
    )

  metrics <- events %>%
    select(match_id, possession_team.name, PlayerPossession, PlayerPossessionTime) %>%
    rename(team.name = possession_team.name) %>%
    distinct() %>%
    na.omit() %>%
    group_by(match_id, team.name) %>%
    summarise(avg.player.possession = mean(PlayerPossessionTime)) %>%
    full_join(metrics)

  metrics <- events %>%
    select(match_id, possession_team.name, OffensivePhase, TimeInLastDefensivePhase) %>%
    rename(team.name = possession_team.name) %>%
    distinct() %>%
    na.omit() %>%
    group_by(match_id, team.name) %>%
    summarise(avg.recovery.time = mean(TimeInLastDefensivePhase)) %>%
    full_join(metrics)

  metrics
}

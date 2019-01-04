library(data.table)
library(dplyr)

make.features <- function(events) {
  events.phased <- events %>%
    group_by(OffensivePhase = rleid(match_id, period, possession_team.id)) %>%
    mutate(TimeInOffensivePhase = max(ElapsedTime) - min(ElapsedTime)) %>%
    ungroup()

  offensive.phases <- unique(events.phased[, c("match_id", "possession_team.name", "OffensivePhase", "TimeInOffensivePhase")])
  row.names(offensive.phases) <- offensive.phases$OffensivePhase

  events.phased$TimeInLastDefensivePhase <- add_row(
    offensive.phases[events.phased$OffensivePhase - 1, "TimeInOffensivePhase"],
    TimeInOffensivePhase = rep(0, tally(events.phased[events.phased$OffensivePhase == 1, ])$n),
    .before = 1
  )$TimeInOffensivePhase

  first.period.phases <- events.phased %>%
    group_by(match_id, period) %>%
    summarize(FirstPhase = first(OffensivePhase)) %>%
    pull(FirstPhase)
  events.phased[events.phased$OffensivePhase %in% first.period.phases, "TimeInLastDefensivePhase"] <- 0

  offensive.phases <- offensive.phases %>%
    group_by(match_id) %>%
    mutate(TotalPossessionTime = cumsum(TimeInOffensivePhase) - TimeInOffensivePhase) %>%
    group_by(possession_team.name, add = TRUE) %>%
    mutate(TimeInPreviousOffensivePhases = cumsum(TimeInOffensivePhase) - TimeInOffensivePhase) %>%
    ungroup()

  events.phased <- left_join(events.phased, offensive.phases) %>%
    mutate(PossessionRate = (TimeInPreviousOffensivePhases + TimeInPoss) / (TotalPossessionTime + TimeInPoss))

  events.phased <- events.phased %>%
    filter(team.id == possession_team.id) %>%
    group_by(PlayerPossession = rleid(OffensivePhase, player.id)) %>%
    transmute(id, PlayerPossessionTime = max(ElapsedTime) - min(ElapsedTime)) %>%
    ungroup() %>%
    right_join(events.phased, "id")

  events.phased
}

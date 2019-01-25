#' @importFrom magrittr "%>%"
#' @importFrom rlang .data

build.metrics <- function(events) {
  metrics <- events %>%
    dplyr::group_by(.data$match_id, .data$team.name) %>%
    dplyr::summarise(
      shots = sum(.data$type.name == "Shot"),
      expulsions = sum(.data$foul_committed.card.name %in% c("Red Card", "Second Yellow")) + sum(.data$bad_behaviour.card.name %in% c("Red Card", "Second Yellow")),
      passes = sum(.data$type.name == "Pass"),
      successful.passes = sum(.data$type.name == "Ball Receipt*"),
      corners = sum(.data$pass.type.name == "Corner", na.rm = TRUE),
      fouls = sum(.data$type.name == "Foul Committed"),
      avg.team.possession = mean(.data$PossessionRate, na.rm = TRUE)
    )

  metrics <- events %>%
    dplyr::select(.data$match_id, .data$possession_team.name, .data$PlayerPossession, .data$PlayerPossessionTime) %>%
    dplyr::rename(team.name = .data$possession_team.name) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::group_by(.data$match_id, .data$team.name) %>%
    dplyr::summarise(avg.player.possession = mean(.data$PlayerPossessionTime)) %>%
    dplyr::full_join(metrics, c("match_id", "team.name"))

  metrics <- events %>%
    dplyr::select(.data$match_id, .data$possession_team.name, .data$OffensivePhase, .data$TimeInLastDefensivePhase) %>%
    dplyr::rename(team.name = .data$possession_team.name) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::group_by(.data$match_id, .data$team.name) %>%
    dplyr::summarise(avg.recovery.time = mean(.data$TimeInLastDefensivePhase)) %>%
    dplyr::full_join(metrics, c("match_id", "team.name"))

  metrics
}

#' @export
build.input.classification <- function(data) {
  metrics <- build.metrics(data$events[data$events$match_id %in% data$matches$match_id, ])
  home.teams <- data$matches %>%
    dplyr::inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name")) %>%
    dplyr::inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    dplyr::mutate(
      women = as.numeric(.data$competition.competition_name != "FIFA World Cup"),
      home = .data$women,
      result = factor(dplyr::case_when(.data$home_score > .data$away_score ~ "win", .data$home_score < .data$away_score ~ "loss", TRUE ~ "draw"))
    )
  away.teams <- data$matches %>%
    dplyr::inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name")) %>%
    dplyr::inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    dplyr::mutate(
      women = as.numeric(.data$competition.competition_name != "FIFA World Cup"),
      home = 0,
      result = factor(dplyr::case_when(.data$home_score < .data$away_score ~ "win", .data$home_score > .data$away_score ~ "loss", TRUE ~ "draw"))
    )
  rbind(home.teams, away.teams) %>% dplyr::select(.data$women, .data$home, .data$result, dplyr::ends_with(".team"), dplyr::ends_with(".adv"))
}

#' @export
build.input.regression <- function(data) {
  metrics <- build.metrics(data$events[data$events$match_id %in% data$matches$match_id, ])
  home.teams <- data$matches %>%
    dplyr::inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name")) %>%
    dplyr::inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    dplyr::mutate(
      women = as.numeric(.data$competition.competition_name != "FIFA World Cup"),
      home = .data$women,
      goal.difference = .data$home_score - .data$away_score
    )
  away.teams <- data$matches %>%
    dplyr::inner_join(metrics, c("match_id", "away_team.away_team_name" = "team.name")) %>%
    dplyr::inner_join(metrics, c("match_id", "home_team.home_team_name" = "team.name"), suffix = c(".team", ".adv")) %>%
    dplyr::mutate(
      women = as.numeric(.data$competition.competition_name != "FIFA World Cup"),
      home = 0,
      goal.difference = .data$away_score - .data$home_score
    )
  rbind(home.teams, away.teams) %>% dplyr::select(.data$women, .data$home, .data$goal.difference, dplyr::ends_with(".team"), dplyr::ends_with(".adv"))
}

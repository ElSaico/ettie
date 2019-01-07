#' @importFrom magrittr "%>%"
#' @importFrom rlang .data

#' @export
compile.dataset <- function() {
  competitions <- StatsBombR::FreeCompetitions()
  matches <- StatsBombR::FreeMatches(competitions$competition_id)
  events <- StatsBombR::StatsBombFreeEvents(matches)
  # TODO unpack lists in lineups$lineup
  # lineups <- StatsBombR::StatsBombFreeLineups(matches)

  events <- StatsBombR::allclean(events)
  goalkeeper.not.null <- events$goalkeeper.end_location != "NULL"
  events$goalkeeper.end_location.x <- purrr::modify_if(events$goalkeeper.end_location, goalkeeper.not.null, dplyr::first)
  events$goalkeeper.end_location.y <- purrr::modify_if(events$goalkeeper.end_location, goalkeeper.not.null, dplyr::last)
  events <- dplyr::select(
    events, -c(
      .data$related_events, .data$location, .data$tactics.lineup, .data$pass.end_location,
      .data$shot.end_location, .data$shot.freeze_frame, .data$goalkeeper.end_location
    )
  )
  events[events == "NULL"] <- NA
  events$goalkeeper.end_location.x <- unlist(events$goalkeeper.end_location.x)
  events$goalkeeper.end_location.y <- unlist(events$goalkeeper.end_location.y)

  list(matches = matches, events = events)
}

#' @export
make.features <- function(events) {
  events.phased <- events %>%
    dplyr::group_by(OffensivePhase = data.table::rleid(.data$match_id, .data$period, .data$possession_team.id)) %>%
    dplyr::mutate(TimeInOffensivePhase = max(.data$ElapsedTime) - min(.data$ElapsedTime)) %>%
    dplyr::ungroup()

  offensive.phases <- unique(events.phased[, c("match_id", "possession_team.name", "OffensivePhase", "TimeInOffensivePhase")])
  row.names(offensive.phases) <- offensive.phases$OffensivePhase

  events.phased$TimeInLastDefensivePhase <- tibble::add_row(
    offensive.phases[events.phased$OffensivePhase - 1, "TimeInOffensivePhase"],
    TimeInOffensivePhase = rep(0, dplyr::tally(events.phased[events.phased$OffensivePhase == 1, ])$n),
    .before = 1
  )$TimeInOffensivePhase

  first.period.phases <- events.phased %>%
    dplyr::group_by(.data$match_id, .data$period) %>%
    dplyr::summarize(FirstPhase = dplyr::first(.data$OffensivePhase)) %>%
    dplyr::pull(.data$FirstPhase)
  events.phased[events.phased$OffensivePhase %in% first.period.phases, "TimeInLastDefensivePhase"] <- 0

  offensive.phases <- offensive.phases %>%
    dplyr::group_by(.data$match_id) %>%
    dplyr::mutate(TotalPossessionTime = cumsum(.data$TimeInOffensivePhase) - .data$TimeInOffensivePhase) %>%
    dplyr::group_by(.data$possession_team.name, add = TRUE) %>%
    dplyr::mutate(TimeInPreviousOffensivePhases = cumsum(.data$TimeInOffensivePhase) - .data$TimeInOffensivePhase) %>%
    dplyr::ungroup()

  events.phased <- dplyr::left_join(events.phased, offensive.phases) %>%
    dplyr::mutate(PossessionRate = (.data$TimeInPreviousOffensivePhases + .data$TimeInPoss) / (.data$TotalPossessionTime + .data$TimeInPoss))

  events.phased <- events.phased %>%
    dplyr::filter(.data$team.id == .data$possession_team.id) %>%
    dplyr::group_by(PlayerPossession = data.table::rleid(.data$OffensivePhase, .data$player.id)) %>%
    dplyr::transmute(.data$id, PlayerPossessionTime = max(.data$ElapsedTime) - min(.data$ElapsedTime)) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(events.phased, "id")

  events.phased
}

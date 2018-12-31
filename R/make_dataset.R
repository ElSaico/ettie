library(dplyr)
library(fst)
library(future.apply)
library(jsonlite)
library(StatsBombR)

make.dataset <- function() {
  plan(multiprocess)

  competitions <- fromJSON('data/statsbomb/data/competitions.json')

  match.files <- dir('data/statsbomb/data/matches', full.names=TRUE)
  matches <- bind_rows(future_lapply(match.files, fromJSON, flatten=TRUE))

  load.events <- function(match) {
    sprintf('data/statsbomb/data/events/%d.json', as.numeric(match['match_id'])) %>%
      fromJSON(flatten=TRUE) %>%
      mutate(
        match_id=as.numeric(match['match_id']),
        competition_id=as.numeric(match['competition.competition_id']),
        season_id=as.numeric(match['season.season_id'])
      )
  }
  events <- bind_rows(future_apply(matches, 1, load.events))

  events.clean <- allclean(events)
  events.clean$goalkeeper.end_location.x <- events.clean$goalkeeper.end_location %>% modify_if(. != 'NULL', first)
  events.clean$goalkeeper.end_location.y <- events.clean$goalkeeper.end_location %>% modify_if(. != 'NULL', last)
  events.selected <- select(
    events.clean, -c(related_events, location, tactics.lineup, pass.end_location,
                     shot.end_location, shot.freeze_frame, goalkeeper.end_location)
  )
  events.selected[events.selected == 'NULL'] <- NA
  events.selected$goalkeeper.end_location.x <- unlist(events.selected$goalkeeper.end_location.x)
  events.selected$goalkeeper.end_location.y <- unlist(events.selected$goalkeeper.end_location.y)

  write.fst(events.selected, format(Sys.Date(), 'data/events-%Y%m%d.fst'))
}

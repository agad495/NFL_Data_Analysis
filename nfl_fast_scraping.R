library(devtools)
devtools::install_github("mrcaseb/nflfastR")
library(nflfastR)
library(tidyverse)
library(DBI)
library(RSQLite)

#Using a database to access data (may not always include latest variables):
update_db()
connection <- dbConnect(SQLite(), "./pbp_db")
pbp_db <- tbl(connection, "nflfastR_pbp")

dbDisconnect(connection)

#Using archived RDS files to access data (this includes newest variables):
seasons = 2006:2020
pbp = purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#Only one way (that I know of) to access roster data:
roster = readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds"))

#Schedules:
seasons = 2016:2020
schedules = purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/schedules/sched_{x}.rds")
    )
  )
})

write_csv(schedules, "python_work/baseball/mysite/nfl/static/nfl/nfl_schedules.csv")

#All games and results from 1999-present:
games = read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv") %>% 
  mutate(
    total_line = case_when(
      game_id == '2006_05_TEN_IND' ~ 47.5,
      game_id == '2006_05_DET_MIN' ~ 41,
      game_id == '2006_19_SEA_CHI' ~ 37.5,
      TRUE ~ total_line
    )
  )
  
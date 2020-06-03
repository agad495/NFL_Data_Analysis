library(devtools)
devtools::install_github("maksimhorowitz/nflscrapR")
devtools::install_github("mrcaseb/nflfastR")
library(nflscrapR)
library(nflfastR)
library(tidyverse)

seasons = 2000:2019
pbp = purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>%
  mutate(
    defteam = case_when(
      (posteam == defteam) & (posteam == home_team) ~ away_team,
      (posteam == defteam) & (posteam == away_team) ~ home_team,
      TRUE ~ defteam
    )
  )

roster = readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds"))

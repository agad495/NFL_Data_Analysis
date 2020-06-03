library(tidyverse)
library(ggimage)

pf = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam)) %>%
  group_by(posteam, game_id, season) %>%
  summarise(Points = max(posteam_score_post)) %>%
  group_by(posteam, season) %>%
  summarise(totPoints = sum(Points)) %>%
  mutate(
    PPG = totPoints / 16,
    totPoints = NULL
  )

pf1 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam)) %>%
  group_by(posteam, game_id, season) %>%
  summarise(Points = max(posteam_score_post)) %>%
  group_by(posteam, season) %>%
  top_n(-8, wt=game_id) %>%
  summarise(totPoints = sum(Points), games=n()) %>%
  mutate(
    PPG = totPoints / games,
    totPoints = NULL,
    games = NULL
  )

pf2 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam)) %>%
  group_by(posteam, game_id, season) %>%
  summarise(Points = max(posteam_score_post)) %>%
  group_by(posteam, season) %>%
  top_n(8, wt=game_id) %>%
  summarise(totPoints = sum(Points), games=n()) %>%
  mutate(
    PPG = totPoints / games,
    totPoints = NULL,
    games = NULL
  )

epa_stats = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1)) %>%
  group_by(posteam, season) %>%
  summarise(epaPlay = mean(epa, na.rm=TRUE), sr = mean(success, na.rm=TRUE))

drives = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1 | qb_kneel == 1)) %>%
  mutate(
    drive_td = if_else(drive_how_ended == 'Touchdown', 1, 0),
    firstand10 = if_else(down == 1, 1, 0),
    drive_killed = if_else(drive_how_ended == 'Touchdown', 0, 1, missing = 1)
  ) %>%
  group_by(posteam, season, week, drive) %>%
  summarise(plays = mean(drive_play_count), EPA=sum(epa, na.rm=TRUE), successes=sum(success, na.rm=TRUE), 
            WPA=sum(wpa, na.rm=TRUE), first_downs=sum(first_down, na.rm=TRUE), kneels=sum(qb_kneel, na.rm=TRUE),
            drive_dead=mean(drive_killed, na.rm=TRUE)) %>%
  mutate(
    drive_success_wpa = if_else(WPA > 0, 1, 0),
    drive_success_epa = if_else(EPA > 0, 1, 0), 
    first_downs = if_else(kneels > 0, 0, first_downs),
    drive_dead = if_else(kneels > 0, 0, drive_dead)
  ) %>%
  group_by(posteam, season) %>%
  summarise(plays=mean(plays), wpa=mean(WPA), 
            ds_wpa=mean(drive_success_wpa), ds_epa=mean(drive_success_epa),
            ds_old=sum(first_downs) / sum(first_downs, drive_dead)) %>%
  left_join(pf, by=c('posteam', 'season')) %>%
  left_join(epa_stats, by=c('posteam', 'season')) %>%
  mutate(
    next_pf = lead(PPG, 1)
  )

drives1 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1)) %>%
  mutate(
    drive_td = if_else(drive_how_ended == 'Touchdown', 1, 0),
    firstand10 = if_else(down == 1, 1, 0),
    drive_killed = if_else(drive_how_ended == 'Touchdown', 0, 1, missing = 1)
  ) %>%
  group_by(posteam, season, week, drive) %>%
  summarise(plays = mean(drive_play_count), EPA=sum(epa, na.rm=TRUE), successes=sum(success, na.rm=TRUE), 
            WPA=sum(wpa, na.rm=TRUE), first_downs=sum(first_down, na.rm=TRUE), kneels=sum(qb_kneel, na.rm=TRUE),
            drive_dead=mean(drive_killed, na.rm=TRUE)) %>%
  mutate(
    drive_success_wpa = if_else(WPA > 0, 1, 0),
    drive_success_epa = if_else(EPA > 0, 1, 0), 
    first_downs = if_else(kneels > 0, 0, first_downs),
    drive_dead = if_else(kneels > 0, 0, drive_dead)
  ) %>%
  group_by(posteam, season, week) %>%
  summarise(plays=sum(plays), epa=sum(EPA), wpa=sum(WPA),
            first_downs=sum(first_downs), ds_wpa=sum(drive_success_wpa),
            ds_epa=sum(drive_success_epa), drive_dead=sum(drive_dead), first_downs=sum(first_downs), 
            drives=n()) %>%
  group_by(posteam, season) %>%
  top_n(-8, wt=week) %>%
  summarise(ds_wpa = sum(ds_wpa)/sum(drives), ds_epa = sum(ds_epa)/sum(drives),
            ds_old = sum(first_downs) / sum(first_downs, drive_dead), drives = sum(drives)) %>%
  left_join(pf1, by=c('posteam', 'season'))

drives2 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1)) %>%
  mutate(
    drive_td = if_else(drive_how_ended == 'Touchdown', 1, 0),
    firstand10 = if_else(down == 1, 1, 0),
    drive_killed = if_else(drive_how_ended == 'Touchdown', 0, 1, missing = 1)
  ) %>%
  group_by(posteam, season, week, drive) %>%
  summarise(plays = mean(drive_play_count), EPA=sum(epa, na.rm=TRUE), successes=sum(success, na.rm=TRUE), 
            WPA=sum(wpa, na.rm=TRUE), first_downs=sum(first_down, na.rm=TRUE), kneels=sum(qb_kneel, na.rm=TRUE),
            drive_dead=mean(drive_killed, na.rm=TRUE)) %>%
  mutate(
    drive_success_wpa = if_else(WPA > 0, 1, 0),
    drive_success_epa = if_else(EPA > 0, 1, 0), 
    first_downs = if_else(kneels > 0, 0, first_downs),
    drive_dead = if_else(kneels > 0, 0, drive_dead)
  ) %>%
  group_by(posteam, season, week) %>%
  summarise(plays=sum(plays), epa=sum(EPA), wpa=sum(WPA),
            first_downs=sum(first_downs), ds_wpa=sum(drive_success_wpa),
            ds_epa=sum(drive_success_epa), drive_dead=sum(drive_dead), first_downs=sum(first_downs), 
            drives=n()) %>%
  group_by(posteam, season) %>%
  top_n(8, wt=week) %>%
  summarise(ds_wpa = sum(ds_wpa)/sum(drives), ds_epa = sum(ds_epa)/sum(drives),
            ds_old = sum(first_downs) / sum(first_downs, drive_dead), drives = sum(drives)) %>%
  left_join(pf2, by=c('posteam', 'season'))

epa1 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1)) %>%
  group_by(posteam, season, week) %>%
  summarise(EPA = sum(epa, na.rm=TRUE), succ = sum(success, na.rm=TRUE), plays = n()) %>%
  group_by(posteam, season) %>%
  top_n(-8, wt=week) %>%
  summarise(EPA = sum(EPA), succ = sum(succ), plays = sum(plays)) %>%
  mutate(
    epaPlay = EPA / plays,
    sr = succ / plays
  ) %>%
  left_join(pf1, by=c('posteam', 'season'))

epa2 = pbp %>%
  filter(season_type == 'REG' & season >= 2006 & !is.na(posteam) & (pass == 1 | rush == 1)) %>%
  group_by(posteam, season, week) %>%
  summarise(EPA = sum(epa, na.rm=TRUE), succ = sum(success, na.rm=TRUE), plays = n()) %>%
  group_by(posteam, season) %>%
  top_n(8, wt=week) %>%
  summarise(EPA = sum(EPA), succ = sum(succ), plays = sum(plays)) %>%
  mutate(
    epaPlay = EPA / plays,
    sr = succ / plays
  ) %>%
  left_join(pf2, by=c('posteam', 'season'))

cor(drives1$ds_wpa, drives2$ds_wpa)^2
cor(drives1$ds_epa, drives2$ds_epa)^2
cor(drives1$ds_old, drives2$ds_old)^2
cor(drives$ds_wpa, drives$PPG)^2
cor(drives$ds_epa, drives$PPG)^2
cor(drives$ds_old, drives$PPG)^2
cor(drives$epaPlay, drives$PPG)^2
cor(drives$sr, drives$PPG)^2
cor(epa1$epaPlay, epa2$epaPlay)^2
cor(epa1$sr, epa2$sr)^2
cor(drop_na(drives)$ds_wpa, drop_na(drives)$next_pf)^2
cor(drop_na(drives)$ds_epa, drop_na(drives)$next_pf)^2
cor(drop_na(drives)$ds_old, drop_na(drives)$next_pf)^2
cor(drop_na(drives)$epaPlay, drop_na(drives)$next_pf)^2
cor(drives1$ds_wpa, drives2$PPG)^2
cor(drives1$ds_epa, drives2$PPG)^2
cor(drives1$ds_old, drives2$PPG)^2
cor(epa1$epaPlay, epa2$PPG)^2
cor(epa1$sr, epa2$PPG)^2

drives %>%
  filter(!is.na(next_pf)) %>%
  ggplot(aes(x=ds_wpa, y=next_pf)) +
  geom_point(aes(color = season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019',
    x = 'Percent of Drives That Increase Win Probability',
    y = "Next Season's Points per Game",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.6, y = 15, label='italic(R) ^ 2 == 0.1710', parse=TRUE
  ) +
  ggsave('nfl_dsr_wpa_predppg.png')

drives %>%
  filter(!is.na(next_pf)) %>%
  ggplot(aes(x=ds_old, y=next_pf)) +
  geom_point(aes(color = season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019',
    x = 'Percent of Down Series Resulting in 1st Down or TD',
    y = "Next Season's Points per Game",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.78, y = 15, label='italic(R) ^ 2 == 0.1554', parse=TRUE
  ) +
  ggsave('nfl_dsr_old_predppg.png')

ggplot(data = NULL, aes(x=drives1$ds_wpa, y=drives2$ds_wpa)) +
  geom_point(aes(color = drives1$season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019 | Drive Success Rate = Percent of Drives That Increase Win %',
    x = 'Drive Success Rate, 1st Half of Season',
    y = "Drive Success Rate, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.65, y = 0.25, label='italic(R) ^ 2 == 0.3014', parse=TRUE
  ) +
  ggsave('nfl_dsr_wpa_stab.png')

ggplot(data = NULL, aes(x=drives1$ds_old, y=drives2$ds_old)) +
  geom_point(aes(color = drives1$season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019 | Drive Success Rate = Percent of Down Series Ending in 1st Down or TD',
    x = 'Drive Success Rate, 1st Half of Season',
    y = "Drive Success Rates, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.8, y = 0.6, label='italic(R) ^ 2 == 0.3569', parse=TRUE
  ) +
  ggsave('nfl_dsr_old_stab.png')

ggplot(data = NULL, aes(x=drives1$ds_wpa, y=drives2$PPG)) +
  geom_point(aes(color = drives1$season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019 | Drive Success Rate = Percent of Drives That Increase Win %',
    x = 'Drive Success Rate, 1st Half of Season',
    y = "Points per Game, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.65, y = 20, label='italic(R) ^ 2 == 0.2654', parse=TRUE
  ) +
  ggsave('nfl_dsr_wpa_ppg.png')

ggplot(data = NULL, aes(x=drives1$ds_old, y=drives2$PPG)) +
  geom_point(aes(color = drives1$season)) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2006-2019 | Drive Success Rate = Percent of Down Series Ending in 1st Down or TD',
    x = 'Drive Success Rate, 1st Half of Season',
    y = "Points per Game, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.8, y = 020, label='italic(R) ^ 2 == 0.2134', parse=TRUE
  ) +
  ggsave('nfl_dsr_old_ppg.png')

drives %>%
  filter(!is.na(next_pf)) %>%
  ggplot(aes(x=epaPlay, y=next_pf)) +
  geom_point(aes(color = season)) +
  labs(
    title = 'EPA per Play',
    subtitle = 'Regular Season 2006-2019',
    x = 'EPA per Play',
    y = "Next Season's Points per Game",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.15, y = 15, label='italic(R) ^ 2 == 0.1541', parse=TRUE
  ) +
  ggsave('nfl_epa_predppg.png')

ggplot(data = NULL, aes(x=epa1$epaPlay, y=epa2$epaPlay)) +
  geom_point(aes(color = epa1$season)) +
  labs(
    title = 'EPA per Play',
    subtitle = 'Regular Season 2006-2019',
    x = 'EPA per Play, 1st Half of Season',
    y = "EPA per Play, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.15, y = -0.2, label='italic(R) ^ 2 == 0.3563', parse=TRUE
  ) +
  ggsave('nfl_epa_stab.png')

ggplot(data = NULL, aes(x=epa1$epaPlay, y=epa2$PPG)) +
  geom_point(aes(color = epa1$season)) +
  labs(
    title = 'EPA per Play',
    subtitle = 'Regular Season 2006-2019',
    x = 'EPA per Play, 1st Half of Season',
    y = "Points per Game, 2nd Half of Season",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  annotate(
    'text', x = 0.2, y = 12.5, label='italic(R) ^ 2 == 0.2611', parse=TRUE
  ) +
  ggsave('nfl_epa_ppg.png')

drives %>%
  filter(season == 2019) %>%
  left_join(teams_colors_logos[,c('team_abbr', 'team_logo_espn')], by=c('posteam'='team_abbr')) %>%
  ggplot(aes(x=ds_wpa, y=ds_old)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  labs(
    title = 'Drive Success Rate',
    subtitle = 'Regular Season 2019',
    x = 'Percent of Drives That Increase Win Probability',
    y = "Percent of Down Series Resulting in 1st Down or TD",
    caption = '@AG_8 | Data: @nflfastR',
    color = 'Season'
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
  ) +
  ggsave('nfl_dsr_2019.png')

library(cfbfastR)
library(tidyverse)
library(dplyr)
library(purrr)

conferences = cfbd_conferences()
pull_all_conference_player_stats <- function(year, start_week = 1, end_week = 15) {
  
  confs <- cfbd_conferences() %>% 
    filter(classification == "fbs") %>% 
    pull(abbreviation) %>% 
    unique()
  
  all_stats <- map_dfr(confs, function(conf) {
    message("Pulling: ", conf)
    
    tryCatch(
      cfbd_stats_season_player(
        year = year,
        conference = conf,
        start_week = start_week,
        end_week = end_week
      ) %>% 
        mutate(conference = conf),
      error = function(e) NULL
    )
  })
  
  return(all_stats)
}

player_stats_2018 = pull_all_conference_player_stats(year = 2018)
player_stats_2019 = pull_all_conference_player_stats(year = 2019)
player_stats_2014 = pull_all_conference_player_stats(year = 2014)
player_stats_2015 = pull_all_conference_player_stats(year = 2015)
player_stats_2016 = pull_all_conference_player_stats(year = 2016)
player_stats_2017 = pull_all_conference_player_stats(year = 2017)
player_stats_2020 = pull_all_conference_player_stats(year = 2020)
player_stats_2021 = pull_all_conference_player_stats(year = 2021)
player_stats_2022 = pull_all_conference_player_stats(year = 2022)
player_stats_2023 = pull_all_conference_player_stats(year = 2023)
player_stats_2024 = pull_all_conference_player_stats(year = 2024)
player_stats_2025 = pull_all_conference_player_stats(year = 2025)


player_stats_all = bind_rows(player_stats_2014,player_stats_2015, player_stats_2016, player_stats_2017, player_stats_2018,player_stats_2019,player_stats_2020,player_stats_2021,player_stats_2022,player_stats_2023,player_stats_2024,player_stats_2025)




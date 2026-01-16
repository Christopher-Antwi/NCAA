library("tidyverse")
library("pacman")
library("knitr")
library(writexl)
library(DT)
library(readxl)
p_load(dplyr)
library(viridis)

# Original Simulation Data Generation Process
n_tiers = 3
teams_per_tier = 10
n_teams = n_tiers* teams_per_tier
alpha = 1
beta = 0.5
noise = 1


tier_means = c(1.5,1.0,0.5)
tier_means_parity = c(1.0,1.0,1.0)
ability_var = 1
k_promote = 2
k_relegate = 2
promo_effect = 0.2
relegation_effect = 0.2
num_seasons = 100

ncaa_no_relegation = function(){
  tibble(
    team_id = 1:n_teams,
    tier = rep(1:n_tiers, each = teams_per_tier)
  ) %>%
    mutate(
      ability = rnorm(dplyr::n(),
                      mean = tier_means[tier],
                      sd = ability_var)
    )
}

simulate_season = function(teams, games_per_season = 320){
  teams %>%
    mutate(
      eps = rnorm(n(), 0, noise),
      performance = ability + eps,
      win_prob = 1/(1+exp(-performance)),
      wins = rbinom(n(),size = games_per_season, prob = win_prob),
      win_rate = wins/games_per_season
    ) %>%
    arrange(desc(performance))%>%
    mutate(rank=row_number())
}


update_tiers = function(season_df) {
  
  # top k in each tier (except tier 1) get promoted
  promos = season_df %>%
    filter(tier > 1) %>%
    group_by(tier) %>%
    arrange(rank, .by_group = TRUE) %>%
    slice(1:k_promote) %>%
    ungroup() %>%
    mutate(
      tier    = tier - 1,
      ability = ability + promo_effect
    )
  
  # bottom k in each tier (except bottom) get relegated
  relegs = season_df %>%
    filter(tier < n_tiers) %>%
    group_by(tier) %>%
    arrange(desc(rank), .by_group = TRUE) %>%
    slice(1:k_relegate) %>%
    ungroup() %>%
    mutate(
      tier    = tier + 1,
      ability = ability - relegation_effect
    )
  
  moved_ids = c(promos$team_id, relegs$team_id)
  
  stay = season_df %>%
    filter(!team_id %in% moved_ids) %>%
    select(team_id, tier, ability)
  
  next_teams = bind_rows(stay,
                         promos %>% select(team_id, tier, ability),
                         relegs %>% select(team_id, tier, ability)) %>%
    arrange(tier, team_id)
  
  next_teams
}


run_league = function(do_relegation = TRUE) {
  teams = ncaa_no_relegation()
  out   = vector("list", num_seasons)
  
  for (t in 1:num_seasons) {
    season = simulate_season(teams) %>%
      mutate(season = t)
    
    out[[t]] = season
    
    if (do_relegation) {
      teams = update_tiers(season)
    }
    # if !do_relegation: tiers and ability stay as is
  }
  
  bind_rows(out)
}

# model A: NO relegation
sim_no_rel   = run_league(do_relegation = FALSE)

# model B: WITH relegation
sim_with_rel = run_league(do_relegation = TRUE)

# quick example comparisons
sim_no_rel  %>% group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()

sim_with_rel %>% group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()

# Ability Data Generating Process Code
n_tiers = 3
teams_per_tier = 10
n_teams = n_tiers* teams_per_tier
alpha = 1
beta = 0.5
noise = 1


tier_means = c(1.5,1.0,0.5)
tier_means_parity = c(1.0,1.0,1.0)
ability_var = 1
k_promote = 2
k_relegate = 2
promo_effect = 0.2
relegation_effect = 0.2
num_seasons = 100

ncaa_no_relegation = function(){
  tibble(
    team_id = 1:n_teams,
    tier = rep(1:n_tiers, each = teams_per_tier)
  ) %>%
    mutate(
      ability = rnorm(dplyr::n(),
                      mean = tier_means[tier],
                      sd = ability_var)
    )
}

simulate_season = function(teams, games_per_season = 320){
  teams %>%
    mutate(
      eps = rnorm(n(), 0, noise),
      performance = ability + eps,
      win_prob = 1/(1+exp(-performance)),
      wins = rbinom(n(),size = games_per_season, prob = win_prob),
      win_rate = wins/games_per_season
    ) %>%
    arrange(desc(performance))%>%
    mutate(rank=row_number())
}


update_tiers = function(season_df) {
  
  # top k in each tier (except tier 1) get promoted
  promos = season_df %>%
    filter(tier > 1) %>%
    group_by(tier) %>%
    arrange(rank, .by_group = TRUE) %>%
    slice(1:k_promote) %>%
    ungroup() %>%
    mutate(
      tier    = tier - 1,
      ability = ability + promo_effect
    )
  
  # bottom k in each tier (except bottom) get relegated
  relegs = season_df %>%
    filter(tier < n_tiers) %>%
    group_by(tier) %>%
    arrange(desc(rank), .by_group = TRUE) %>%
    slice(1:k_relegate) %>%
    ungroup() %>%
    mutate(
      tier    = tier + 1,
      ability = ability - relegation_effect
    )
  
  moved_ids = c(promos$team_id, relegs$team_id)
  
  stay = season_df %>%
    filter(!team_id %in% moved_ids) %>%
    select(team_id, tier, ability)
  
  next_teams = bind_rows(stay,
                         promos %>% select(team_id, tier, ability),
                         relegs %>% select(team_id, tier, ability)) %>%
    arrange(tier, team_id)
  
  next_teams
}


run_league = function(do_relegation = TRUE) {
  teams = ncaa_no_relegation()
  out   = vector("list", num_seasons)
  
  for (t in 1:num_seasons) {
    season = simulate_season(teams) %>%
      mutate(season = t)
    
    out[[t]] = season
    
    if (do_relegation) {
      teams = update_tiers(season)
    }
    # if !do_relegation: tiers and ability stay as is
  }
  
  bind_rows(out)
}

# model A: NO relegation
sim_no_rel   = run_league(do_relegation = FALSE)

# model B: WITH relegation
sim_with_rel = run_league(do_relegation = TRUE)

# quick example comparisons
sim_no_rel  %>% group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()

sim_with_rel %>% group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()
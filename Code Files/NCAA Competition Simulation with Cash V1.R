library("tidyverse")
library("pacman")
library("knitr")
library(writexl)
library(DT)
library(readxl)
p_load(dplyr)
library(viridis)
library(lubridate)

# Cash Run DGP

n_tiers = 3
teams_per_tier = 10
n_teams = n_tiers * teams_per_tier

noise = 1
ability_var = 1

tier_means = c(1.5, 1.0, 0.5)

k_promote  = 2
k_relegate = 2

promo_effect      = 0.2
relegation_effect = 0.2

num_seasons = 100


ncaa_cash = function(){
  tibble(
    team_id = 1:n_teams,
    tier    = rep(1:n_tiers, each = teams_per_tier),
    
    # fixed attributes
    acad_rank = sample(1:5, n_teams, replace = TRUE),  #
    pop_cat   = sample(c("Small","Medium","Large"),
                       n_teams, replace = TRUE,
                       prob = c(0.50, 0.35, 0.15))
  ) %>%
    rowwise() %>%
    mutate(
      pop = case_when(
        pop_cat == "Small"  ~ sample(3000:8000,   1),
        pop_cat == "Medium" ~ sample(8500:18000,  1),
        pop_cat == "Large"  ~ sample(18500:55000, 1)
      )
    ) %>%
    ungroup() %>%
    mutate(
      # baseline cash from academics + population
      cash = 50 +
        15 * scale(acad_rank)[,1] +
        20 * scale(log(pop))[,1] +
        rnorm(n_teams, 0, 5),
      
      # initial ability for cash model is dependant on cash
      ability_cash = tier_means[tier] +
        0.35 * scale(cash)[,1] +
        rnorm(n_teams, 0, ability_var)
    )
}



simulate_season_cash = function(teams, games_per_season = 320){
  teams %>%
    mutate(
      eps = rnorm(n(), 0, noise),
      performance = ability_cash + eps,
      win_prob = 1/(1+exp(-performance)),
      wins = rbinom(n(), size = games_per_season, prob = win_prob),
      win_rate = wins/games_per_season
    ) %>%
    arrange(desc(performance)) %>%
    mutate(rank = row_number())
}


update_cash_ability_cash = function(season_df){
  season_df %>%
    mutate(
      cash = cash +
        0.08 * wins +
        0.50 * scale(acad_rank)[,1] +
        0.50 * scale(log(pop))[,1] +
        rnorm(n(), 0, 2),
      
      ability_cash = ability_cash +
        0.15 * scale(cash)[,1] +
        rnorm(n(), 0, 0.2)
    )
}


# Promotion/Relegation update for CASH model
# (preserves ability_cash + cash + attributes)

update_tiers_cash = function(season_df){
  
  state_cols = c("team_id","tier","ability_cash","acad_rank","pop_cat","pop","cash")
  
  promos = season_df %>%
    filter(tier > 1) %>%
    group_by(tier) %>%
    arrange(rank, .by_group = TRUE) %>%
    slice(1:k_promote) %>%
    ungroup() %>%
    mutate(
      tier         = tier - 1,
      ability_cash = ability_cash + promo_effect
    ) %>%
    select(any_of(state_cols))
  
  relegs = season_df %>%
    filter(tier < n_tiers) %>%
    group_by(tier) %>%
    arrange(desc(rank), .by_group = TRUE) %>%
    slice(1:k_relegate) %>%
    ungroup() %>%
    mutate(
      tier         = tier + 1,
      ability_cash = ability_cash - relegation_effect
    ) %>%
    select(any_of(state_cols))
  
  moved_ids = c(promos$team_id, relegs$team_id)
  
  stay = season_df %>%
    filter(!team_id %in% moved_ids) %>%
    select(any_of(state_cols))
  
  bind_rows(stay, promos, relegs) %>%
    arrange(tier, team_id)
}


# Run league for CASH model

run_league_cash = function(do_relegation = TRUE){
  
  teams = ncaa_cash()
  out   = vector("list", num_seasons)
  
  for (t in 1:num_seasons){
    
    season = simulate_season_cash(teams) %>%
      mutate(season = t)
    
    season_updated = update_cash_ability_cash(season)
    
    out[[t]] = season_updated
    
    if (do_relegation) {
      teams = update_tiers_cash(season_updated)
    } else {
      teams = season_updated %>%
        select(team_id, tier, ability_cash, acad_rank, pop_cat, pop, cash) %>%
        arrange(tier, team_id)
    }
  }
  
  bind_rows(out)
}


sim_no_rel_cash   <- run_league_cash(do_relegation = FALSE)
sim_with_rel_cash <- run_league_cash(do_relegation = TRUE)

# quick check
sim_no_rel_cash %>%
  group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()

sim_with_rel_cash %>%
  group_by(season, tier) %>%
  summarise(mean_perf = mean(performance), .groups = "drop") %>%
  head()
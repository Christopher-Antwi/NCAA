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


# 100 Season Run With No Relegation

ggplot(sim_no_rel,
       aes(x = season,
           y = wins,
           group = team_id,
           color = factor(team_id))) +          
  geom_line(alpha = 0.6, linewidth = 1) +
  facet_wrap(~ tier, nrow = 1) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,max(sim_no_rel$season), by = 10)) +
  labs(
    title    = "Team Wins Across Season (No Relegation)",
    subtitle = "Assumptions: Initial Tier Ability: Tier 1 (1.5), Tier 2 (1), Tier 3 (0.5) & Teams Improve After Promotion and Decline After Relegation YOU DON'T HAVE PROMOTION/RELEGATION HERE, THOUGH?",
    x        = "Season",
    y        = "Wins",
    color   = "Team_ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5,
                                 margin = margin(t=8, b=12)),
    legend.position = "none")
# 100 Season Run With Relegation
ggplot(sim_with_rel,
       aes(x = season,
           y = wins,
           group = team_id,
           color = factor(team_id))) +          
  geom_line(alpha = 0.6, linewidth = 1) +
  facet_wrap(~ tier, nrow = 1) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,max(sim_no_rel$season), by = 10)) +
  labs(
    title    = "Team Wins Across Season (With Relegation)",
    subtitle = stringr::str_wrap(
      "Assumptions: Initial Tier Ability: Tier 1 (1.5), Tier 2 (1), Tier 3 (0.5) & Teams Improve After Promotion and Decline After Relegation ",
      width = 80
    ),
    x        = "Season",
    y        = "Wins",
    color   = "Team_ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5,
                                 margin = margin(t=8, b=12)),
    legend.position = "none")



# Ability 100 Season Run No Relegation

ggplot(sim_no_rel,
      aes(x = season,
          y = ability,
          group = team_id,
          color = factor(team_id))) +          
  geom_line(alpha = 0.6, linewidth = 1) +
  facet_wrap(~ tier, nrow = 1) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,max(sim_no_rel$season), by = 10)) +
  labs(
    title    = "Team Ability Across Season (No Relegation)",
    subtitle = "Each team has their individual line & color",
    x        = "Season",
    y        = "Ability",
    color   = "Team_ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5,
                                 margin = margin(t=8, b=12)),
    legend.position = "none")

# Ability 100 Season Run With Relegation
ggplot(sim_with_rel,
       aes(x = season,
           y = ability,
           group = team_id,
           color = factor(team_id))) +          
  geom_line(alpha = 0.6, linewidth = 1) +
  facet_wrap(~ tier, nrow = 1) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,max(sim_no_rel$season), by = 10)) +
  labs(
    title    = "Team Ability Dispersion Across Season (With Relegation)",
    subtitle = stringr::str_wrap(
      "Assumptions: Initial Tier Ability: Tier 1 (1.5), Tier 2 (1), Tier 3 (0.5) & Teams Improve After Promotion and Decline After Relegation  ",
      width = 80
    ),
    x        = "Season",
    y        = "Ability",
    color   = "Team_ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5,
                                 margin = margin(t=8, b=12)),
    legend.position = "none")

# Ability Summary With No Relegation
ability_tier_summary_no_reg = sim_no_rel %>%
  group_by(tier, season) %>%
  summarise(
    ability_mean_no = mean(ability),
    ability_var_no  = var(ability),
    ability_sd_no  = sd(ability),
    .groups = "drop"
  )

ggplot(ability_tier_summary_no_reg, aes(x = season, y = ability_var_no, color = factor(tier))) +
  geom_line(aes(y = ability_mean_no)) +   # lower rank = better
  labs(
    title = "Ability Volatility Over Time (No Relegation)",
    x = "Season",
    y = "Variance of Ability",
    color = "Tier"
  ) +
  theme_minimal()


# Ability Summary With Relegation
ability_tier_summary = sim_with_rel %>%
  group_by(tier, season) %>%
  summarise(
    ability_mean = mean(ability),
    ability_var  = var(ability),
    ability_sd  = sd(ability),
    .groups = "drop"
  )

tier1_var = ability_tier_summary %>%
  filter(tier==1)
tier2_var = ability_tier_summary %>%
  filter(tier==2)
tier3_var = ability_tier_summary %>%
  filter(tier==3)

ggplot(ability_tier_summary, aes(x = season, y = ability_var, color = factor(tier))) +
  geom_line(aes(y = ability_mean)) +   # lower rank = better
  labs(
    title = "Ability Volatility Over Time (With Relegation)",
    x = "Season",
    y = "Variance of Ability",
    color = "Tier"
  ) +
  theme_minimal()

# Tier 1 Variance
ggplot(tier1_var, aes(x = season, y = ability_var)) +
  geom_line(aes(y = ability_mean)) +   # lower rank = better
  labs(
    title = "Tier 1 Ability Volatility Over Time",
    x = "Season",
    y = "Variance of Ability"
  ) +
  theme_minimal()
# Tier 2 Variance
ggplot(tier2_var, aes(x = season, y = ability_var)) +
  geom_line(aes(y = ability_mean)) +   # lower rank = better
  labs(
    title = "Tier 2 Ability Volatility Over Time",
    x = "Season",
    y = "Variance of Ability"
  ) +
  theme_minimal()
# Tier 3 Variance
ggplot(tier3_var, aes(x = season, y = ability_var)) +
  geom_line(aes(y = ability_mean)) +   # lower rank = better
  labs(
    title = "Tier 3 Ability Volatility Over Time",
    x = "Season",
    y = "Variance of Ability"
  ) +
  theme_minimal()
# Tier Movement No Relegation
sim_no_rel %>%
  mutate(
    tier = factor(tier),
    team_id = factor(team_id)
  ) %>%
  ggplot(aes(x = season, y = tier, group = team_id)) +
  geom_line(alpha = 0.2) +
  labs(
    title = "Team Flows Between Tiers Over Time",
    x = "Season",
    y = "Teams"
  ) +
  theme_minimal()
# Tier Movement With Relegation
sim_with_rel_init = sim_with_rel %>%
  arrange(team_id, season) %>%
  group_by(team_id) %>%
  mutate(initial_tier = first(tier)) %>%
  ungroup()


sim_with_rel_init %>%
  mutate(
    tier_num         = as.numeric(tier),
    initial_tier = factor(initial_tier),
    team_id = factor(team_id)
  ) %>%
  ggplot(aes(
    x = season,
    y = tier_num,
    group = team_id,
    color = team_id
  )) +
  geom_line(alpha = 0.75, linewidth = 0.75) +
  facet_wrap(~ initial_tier, ncol = 1,
             labeller = labeller(initial_tier = c('1' = "Tier 1", '2' = "Tier 2", '3' = "Tier 3"))) +
  scale_y_reverse(
    breaks = c(1,2,3),
    labels = c("Tier 1", "Tier 2", "Tier 3")
  ) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Team Flows Between Tiers Over Time\nWith Relegation",
    x = "Season",
    y = "Tier"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
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


# Cash Run Results With Relegation
ggplot(sim_with_rel_cash,
       aes(x = season,
           y = cash,
           group = team_id,
           color = factor(team_id))) +          
  geom_line(alpha = 0.6, linewidth = 1) +
  facet_wrap(~ tier, nrow = 1) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(0,max(sim_no_rel$season), by = 10)) +
  labs(
    title    = "Cash Dispersion Throughout The Seasons (With Relegation)",
    subtitle = stringr::str_wrap(
      "Cash is dependent upon Academic Level & Population Size ",
      width = 80
    ),
    x        = "Season",
    y        = "Cash",
    color   = "Team_ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5,
                                 margin = margin(t=8, b=12)),
    legend.position = "none")



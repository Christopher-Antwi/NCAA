library("tidyverse")
library("pacman")
library("knitr")
library(writexl)
library(DT)
library(readxl)
p_load(dplyr)
library(viridis)

Comp_No_Rel_Seasons_Sim = ggplot(
  sim_no_rel,
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
ggsave(
  filename = "figures/Team Wins Across Season No Relegation.svg",
  plot = Comp_No_Rel_Seasons_Sim,
  width = 9,
  height = 4.5,
  units = "in"
)

# Ability 100 Season Run No Relegation
Ability_No_Rel_Seasons = ggplot(
  sim_no_rel,
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
ggsave(
  filename = "figures/Team Ability Across  Seasons No Relegation.svg",
  plot = Ability_No_Rel_Seasons,
  width = 9,
  height = 4.5,
  units = "in"
)
# Ability Summary With No Relegation
ability_tier_summary_no_reg = sim_no_rel %>%
  group_by(tier, season) %>%
  summarise(
    ability_mean_no = mean(ability),
    ability_var_no  = var(ability),
    ability_sd_no  = sd(ability),
    .groups = "drop"
  )

Ability_Tier_Summary_Plot = ggplot(
  ability_tier_summary_no_reg, aes(x = season, y = ability_var_no, color = factor(tier))) +
  geom_line(aes(y = ability_mean_no)) +   # lower rank = better
  labs(
    title = "Ability Volatility Over Time (No Relegation)",
    x = "Season",
    y = "Variance of Ability",
    color = "Tier"
  ) +
  theme_minimal()
ggsave(
  filename = "figures/Team Ability Across  Seasons No Relegation.svg",
  plot = Ability_Tier_Summary_Plot,
  width = 9,
  height = 4.5,
  units = "in"
)

# Tier "Movement" No Relegation

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


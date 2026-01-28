library(ggplot2)
library(dplyr)
library(lubridate)

player_stats_all %>%
  count(year, position) %>%
  ggplot(aes(x = year, y = n, color = position, group = position)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Number of Players by Position Over Time",
    x = "Season",
    y = "Count",
    color = "Position"
  ) +
  theme_minimal()

offense_position = c("WR", "RB",  "QB" , "FB", "TE" , "C"  ,  "OT"  , "OL" ,"G")
defense_position = c("CB" , "S" , "DB", "DL", "LB" , "DE" ,"LS", "NT", "DT", "OLB", "ILB")    
special_misc_position = c("PR", "EDGE", "ATH", "P", "PK", "?")

plot_position_group = function(data, pos_vec, title_text) {
  data %>%
    filter(position %in% pos_vec) %>%
    count(year, position) %>%
    ggplot(aes(x = year, y = n, color = position, group = position)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = title_text,
      x = "Season",
      y = "Count",
      color = "Position"
    ) +
    theme_minimal()
}

# Offense
plot_offense = plot_position_group(player_stats_all, offense_position, 
                                    "Offensive Positions Over Time")

# Defense
plot_defense = plot_position_group(player_stats_all, defense_position, 
                                    "Defensive Positions Over Time")

# Special / Misc
plot_special = plot_position_group(player_stats_all, special_misc_position, 
                                    "Special Teams / Misc Positions Over Time")

# Print them
plot_offense
plot_defense
plot_special

# Looking at the  identified stats over time

defensive_sacks_freq =  player_stats_all%>%
  group_by(year) %>%
  summarise(avg_sacks_allowed = mean(defensive_sacks, na.rm = TRUE))
rushing_carries_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_rushing_carries = mean(rushing_car, na.rm = TRUE))
rushing_ypc_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_ypc = mean(rushing_ypc, na.rm = TRUE))
rushing_long = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_long_run = mean(rushing_long, na.rm = TRUE))
passing_attmpts_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_pass_attmps = mean(passing_att, na.rm = TRUE))
defensive_tfl_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_tfl_allowed = mean(defensive_tfl, na.rm = TRUE))
defensive_solo_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_solo_tkls = mean(defensive_solo, na.rm = TRUE))
defensive_qb_hurt_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_qb_hurt_allowed = mean(defensive_qb_hur, na.rm = TRUE))
defensive_td_freq = player_stats_all%>%
  group_by(year) %>%
  summarise(avg_dtds_allowed = mean(defensive_td, na.rm = TRUE))

defensive_sacks_freq_plot = ggplot(defensive_sacks_freq, aes(x = year, y = avg_sacks_allowed)) +
  geom_line() +
  labs(title = "Avg Defensive Sacks Allowed Over Time ",
       x = "Year",
       y = "Average Sacks Per Player ")

rushing_carries_freq_plot = ggplot(rushing_carries_freq, aes(x = year, y = avg_rushing_carries)) +
  geom_line() +
  labs(title = "Avg Carries Per RB Over Time ",
       x = "Year",
       y = "Avg Carries Per Player ")

rushing_long_plot = ggplot(rushing_long, aes(x = year, y = avg_long_run)) +
  geom_line() +
  labs(title = "Avg Breakout Run Per Player Over Time ",
       x = "Year",
       y = "Average Long Run (Yards)")

rushing_ypc_freq = ggplot(rushing_ypc_freq, aes(x = year, y = avg_ypc)) +
  geom_line() +
  labs(title = "Avg YPC Over Time ",
       x = "Year",
       y = "Average YPC Per Player (Yards) ")

passing_attmpts_freq_plot = ggplot(passing_attmpts_freq, aes(x = year, y = avg_pass_attmps)) +
  geom_line() +
  labs(title = "Avg Passing AttempTs Over Time ",
       x = "Year",
       y = "Average Passing Attemps Per Player ")

defensive_tfl_freq_plot = ggplot(defensive_tfl_freq, aes(x = year, y = avg_tfl_allowed)) +
  geom_line() +
  labs(title = "Avg TFL's Allowed Over Time ",
       x = "Year",
       y = "Average TFL's Per Player ")

defensive_qb_hurt_freq_plot = ggplot(defensive_qb_hurt_freq, aes(x = year, y = avg_qb_hurt_allowed)) +
  geom_line() +
  labs(title = "QB Hurt Allowed Over Time ",
       x = "Year",
       y = "Average Hurt Inflicted on QB Per Player ")
defensive_solo_freq = ggplot(defensive_solo_freq, aes(x = year, y = avg_solo_tkls)) +
  geom_line() +
  labs(title = "Avg Solo Tackles Over Time ",
       x = "Year",
       y = "Average Solo Tackles Per Player ")
defensive_td_freq_plot = ggplot(defensive_td_freq, aes(x = year, y = avg_dtds_allowed)) +
  geom_line() +
  labs(title = "Avg Defensive TDs Allowed Over Time ",
       x = "Year",
       y = "Average DTD's Per Player ")
ggsave("Images/avg_sacks_plot.png", defensive_sacks_freq_plot, width = 10, height = 6, dpi = 300)
ggsave("Images/avg_solotkl_plot.png", defensive_tfl_freq_plot, width = 10, height = 6, dpi = 300)
ggsave("Images/avg_dtd_plot.png", defensive_td_freq_plot, width = 10, height = 6, dpi = 300)
ggsave("Images/passing_attempts_plot.png", passing_attmpts_freq_plot, width = 10, height = 6, dpi = 300)
ggsave("Images/avg_carries_plot.png", rushing_carries_freq_plot, width = 10, height = 6, dpi = 300)
ggsave("Images/ypc_plot.png", rushing_ypc_freq, width = 10, height = 6, dpi = 300)
ggsave("Images/breakout_run_plot.png", rushing_long_plot, width = 10, height = 6, dpi = 300)


ggsave("Images/off_positions_over_time.png", plot_offense, width = 10, height = 6, dpi = 300)
ggsave("Images/def_positions_over_time.png", plot_defense, width = 10, height = 6, dpi = 300)
ggsave("Images/misc_positions_over_time.png", plot_special, width = 10, height = 6, dpi = 300)


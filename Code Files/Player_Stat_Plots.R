library(ggplot2)
library(dplyr)

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

ggsave("Images/off_positions_over_time.png", plot_offense, width = 10, height = 6, dpi = 300)
ggsave("Images/def_positions_over_time.png", plot_defense, width = 10, height = 6, dpi = 300)
ggsave("Images/misc_positions_over_time.png", plot_special, width = 10, height = 6, dpi = 300)


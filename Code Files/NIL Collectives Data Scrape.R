library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(writexl)
library(tidyverse)

url <- "https://nil-ncaa.com/collectives"
page = read_html(url)
tables = page %>%
  html_elements("table") %>%
  html_table(fill = TRUE)
length(tables)

summary_table = tables[[1]]
funding_table = tables[[2]]
schools_table = tables[[3]]

colnames(schools_table) = schools_table[1, ]
schools_table = schools_table[-1, ]

write_xlsx(schools_table, "Complete Listing of All NIL Collectives by School.xlsx")
colnames(schools_table)[5:6] = c("Collective_2", "Collective_3")



long_collectives <- schools_table %>%
  pivot_longer(
    cols = c(`Collective(s)`, Collective_2, Collective_3),
    names_to = "collective_source",
    values_to = "collective"
  ) %>%
  filter(!is.na(collective), collective != "")

conference_freq <- long_collectives %>%
  count(Conference)





library(ggplot2)
library(dplyr)

conference_freq = long_collectives %>%
  count(Conference, name = "Num_Collectives") %>%
  arrange(desc(Num_Collectives))

Num_Collectives_Graphic = ggplot(conference_freq,
       aes(x = reorder(Conference, Num_Collectives),
           y = Num_Collectives)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Number of NIL Collectives by Conference",
    subtitle = "Including primary and secondary school-affiliated collectives",
    x = "Conference",
    y = "Total Collectives"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )


p5 = c("SEC", "ACC", "Big Ten", "Big 12", "Pac-12")

p5_conference_freq <- conference_freq %>%
  mutate(Power5 = ifelse(Conference %in% p5, "Power 5", "Non-Power 5"))

p5_freq_graphic = ggplot(p5_conference_freq,
       aes(x = reorder(Conference, Num_Collectives),
           y = Num_Collectives,
           fill = Power5)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Power 5" = "#d62728", "Non-Power 5" = "#1f77b4")) +
  labs(
    title = "Distribution of NIL Collectives Across Conferences",
    x = "Conference",
    y = "Total Collectives",
    fill = "Conference Tier"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "NIL_Collectives_by_Conference.png",
  plot = p5_freq_graphic,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave("NIL_Collectives_by_Conference.pdf", p5_freq_graphic,width = 10, height = 6)

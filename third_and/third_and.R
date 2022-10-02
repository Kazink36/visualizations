library(cfbfastR)
library(cfbplotR)
library(ggtext)
library(tidyverse)

pbp <- cfbfastR::load_cfb_pbp(2021)

utah_3rd <- pbp %>%
  filter(pos_team == "Utah", down == 3, pass + rush == 1, distance <= 10) %>%
  group_by(distance) %>%
  summarize(yards = mean(yards_gained),
            n = n()) %>%
  mutate(label = paste0("(",n, ") 3rd and ", distance, "'s"),
         x = -1*distance,
         xend = x+yards,
         label_yards = paste0(round(yards,1), " yards"),
         conversion = ifelse(yards>distance, "convert", "fail"))

utah_3rd %>%
  ggplot(aes(x = x, y = distance)) +
  annotate(GeomCFBlogo, x = 3, y = 7, team = "Utah", height = .5, alpha = .5) +
  geom_vline(xintercept = 0, color = "yellow",size = 2) +
  geom_segment(aes(xend = xend, yend = distance, color = conversion),
               size = 1,

               arrow = arrow(type = "closed")) +
  geom_segment(aes(xend = xend -.35, yend = distance, color = conversion),
               size = 6) +
  geom_text(aes(label = label),nudge_y = .5,nudge_x = 1) +
  geom_text(aes(label = label_yards), nudge_y = .5,nudge_x = 3) +
  labs(title = "<span style='color:#890012'>Utah's</span> Yards Per Play on <span style='color:#7e8083'>3rd Downs</span>",
       subtitle = "2021 Season through Week 12",
       caption = "@JaredDLee | Data: @CFBData via @cfbfastR",
       y = "", x = "Yards to First Down") +
  scale_y_reverse(breaks = 10:1) +
  scale_x_continuous(breaks = -10:7,
                     labels = function(x){ifelse(x==0,"First\nDown",-1*x)}) +
  scale_color_manual(values = c("convert" = "#890012","fail" = "#7e8083")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = .5,face = "bold", size = 20),
        plot.subtitle = element_text(hjust = .5),
        plot.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
        legend.position = "none")
ggsave("third_and.png",width = 10, height = 8)


# Kansas
pbp <- cfbfastR::load_cfb_pbp(2022)

kansas_3rd <- pbp %>%
  filter(pos_team == "Kansas", down == 3, pass + rush == 1, distance <= 10) %>%
  group_by(distance) %>%
  summarize(yards = mean(yards_gained),
            n = n()) %>%
  mutate(label = paste0("(",n, ") 3rd and ", distance, "'s"),
         x = -1*distance,
         xend = x+yards,
         label_yards = paste0(round(yards,1), " yards"),
         conversion = ifelse(yards>distance, "convert", "fail"))

kansas_3rd %>%
  ggplot(aes(x = x, y = distance)) +
  annotate(GeomCFBlogo, x = 11, y = 7, team = "Kansas", height = .5, alpha = .5) +
  geom_vline(xintercept = 0, color = "yellow",size = 2) +
  geom_segment(aes(xend = xend, yend = distance, color = conversion),
               size = 1,

               arrow = arrow(type = "closed")) +
  geom_segment(aes(xend = xend -.35, yend = distance, color = conversion),
               size = 6) +
  geom_text(aes(label = label),nudge_y = .5,nudge_x = 1) +
  geom_text(aes(label = label_yards), nudge_y = .5,nudge_x = 4) +
  labs(title = "<span style='color:#0022B4'>Kansas's</span> Yards Per Play on <span style='color:#e8000d'>3rd Downs</span>",
       subtitle = "2022 Season through week 5",
       caption = "@JaredDLee | Data: @CFBData via @cfbfastR",
       y = "", x = "Yards to First Down") +
  scale_y_reverse(breaks = 10:1) +
  scale_x_continuous(breaks = -10:17,
                     labels = function(x){ifelse(x==0,"First\nDown",-1*x)}) +
  scale_color_manual(values = c("convert" = "#0022B4","fail" = "#e8000d")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = .5,face = "bold", size = 20),
        plot.subtitle = element_text(hjust = .5),
        plot.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
        legend.position = "none")
ggsave("third_and_kansas.png",width = 10, height = 8)

pbp %>%
  filter(year == 2022) |>
  filter(pos_team == "Kansas", down == 3, pass + rush == 1) %>%
  select(game_id,id_play,pos_team,def_pos_team,pos_team_score,def_pos_team_score,period,clock.minutes,clock.seconds,down,distance,yards_to_goal,play_text) |>
  write_csv("third_and_kansas.csv")

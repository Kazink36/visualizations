library(cfbfastR)
library(tidyverse)
library(cfbplotR)
library(glue)

season_stats <- map_df(2018:2021,function(x){
  Sys.sleep(1)
  cfbfastR::cfbd_stats_season_player(x,category = "passing") %>%
    mutate(player_season = paste0(player,"_",x),
           season = x)
  })


plot_data <- season_stats %>%
  filter(passing_att > 100, passing_td >= 10) %>%
  mutate(td_rate = passing_td / passing_att) %>%
  arrange(desc(passing_td)) %>%
  mutate(first = str_extract(player,".+ "),
         last = str_remove(player,".+ "),
         label = ifelse(row_number() <= 5, glue("{last} '{season%%100}"),""),
         school = ifelse(row_number() <= 5, team,NA_character_),
         nudge_x = ifelse(last == "Haskins",20,0),
         color = case_when(label == "Zappe '21" ~"#F32026",
                           label != "" ~ "white",
                           passing_td < 20 ~ "purple",
                           passing_td < 30 ~ "orange",
                           passing_td < 40 ~ "cyan",
                           passing_td < 50 ~ "darkred",
                           TRUE ~ "blue"))

labels <- tribble(
  ~ x, ~ y, ~ label, ~ color,
  20, 13, "10", "purple",
  20, 23, "20","orange",
  20, 33, "30","cyan",
  20, 43, "40","darkred",
  20, 53, "                     50 Passing TDs","#F32026",
  #20, 63, "60",
  0, 60, "0/700 Pass Attempts","white",
  100, 60, "100","white",
  200, 60, "200","white",
  300, 60, "300","white",
  400, 60, "400","white",
  500, 60, "500","white",
  600, 60, "600","white"
)

plot_data %>%
  ggplot(aes(x = passing_att, y = passing_td)) +
  geom_text(data = labels, aes(x = x, y = y, label = label, color = color)) +
  geom_segment(aes(color = color),xend = 0, yend = 0, alpha = 0.7) +
  geom_point(aes(color = color),size = 0.5, alpha = 0.7) +
  geom_cfb_logos(data = plot_data %>% filter(!is.na(school)),aes(team = school, color = NULL),height = 0.05) +
  geom_text(aes(label = label,x = passing_att + nudge_x,color = color), nudge_y = 5, fontface = "bold") +
  #ggrepel::geom_text_repel(aes(label = label)) +
  expand_limits(x = c(0,700), y = c(0,60)) +
  scale_x_continuous(breaks = seq(100,700,100)) +
  scale_y_continuous(breaks = seq(10,60,10)) +
  scale_color_identity() +
  labs(title = "Who threw TDs most frequently?",
       subtitle = "<span style='color:#F32026'>Bailey Zappe</span><span style='color:#ffffff'> set a single season record<br>with 56 passing touchdowns</span>",
       caption = "2018-2021 seasons\n@JaredDLee\nInspiration: @TonyElHabr\nData: @cfbfastR") +
  coord_polar() +
  theme_dark() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "#181818",fill = "#181818"),
        panel.background = element_rect(color = "#181818",fill = "#181818"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(color = "white",size = 26, face = "bold"),
        plot.subtitle = element_markdown(size = 18),
        plot.caption = element_text(color = "white", size = 12)
        )

ggsave("qb_spiral.png",width = 6, height = 8)

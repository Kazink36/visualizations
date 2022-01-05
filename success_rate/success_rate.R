library(cfbfastR)
library(cfbplotR)
library(tidyverse)

utah_stats <- cfbd_stats_game_advanced(
  2021,
  week = NULL,
  team = "Utah",
  opponent = NULL,
  excl_garbage_time = FALSE,
  season_type = "both"
)

results <- cfbd_game_info(2021,team = "Utah",season_type = "both")

results <- results %>%
  mutate(result = case_when(home_team == "Utah" & home_points > away_points ~ "w",
                            away_team == "Utah" & away_points > home_points ~ "w",
                            TRUE ~ "l"),
         label = ifelse(home_team == "Utah",paste(home_points,away_points, sep = " - "),paste(away_points,home_points,sep = " - "))) %>%
  select(game_id,result,label)

plot_data <- utah_stats %>%
  mutate(week = row_number()) %>%
  left_join(results, by = "game_id")

# Basic line plot with points
ggplot(data=plot_data, aes(x=week, y=off_success_rate, group=1)) +
  geom_line(linetype = "dashed",color="#890012") +
  #geom_point() +
  geom_mean_lines(aes(h_var = off_success_rate), color = "#890012") +
  cfbplotR::geom_cfb_logos(aes(team = opponent),height = .075) +
  #cfbplotR::geom_cfb_logos(aes(team = opponent, color = result),height = .075,alpha = .4) +
  geom_label(aes(label = label, color = result), nudge_y =  .05,
             fontface = "bold", size = 4) +
  annotate("text",x = 6.2,y = .35,
           label = "Cam Rising\nStarts", size = 5,fontface = "bold") +
  annotate("curve",x = 5.3,y = .375, xend = 4.1,yend = .475,
           arrow = arrow(length = unit(.15,"inches"),type = "closed"),
           color = "#890012", size = 1.5, curvature = -0.2) +
  annotate(GeomCFBlogo, x = 2.5, y = .7, team = "Utah", height = .24,alpha = .6) +
  labs(title = "<span style='color:#890012'>Utah</span>'s Offensive Success Rate",
       subtitle = "2021 Season by game",
       caption = "Jared Lee | UteZone | Data: collegefootballdata.com",
       x = "Opponent",
       y = "Success Rate") +
  scale_x_continuous(breaks = plot_data$week) +
  scale_y_continuous(limits = c(0,.8),labels = scales::percent,breaks = seq(0,1,.2),minor_breaks = seq(.1,.9,.2)) +
  scale_color_manual(values = c("w" = "#377EB8","l" ="#E41A1C")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),#element_text(angle = 30),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = ggtext::element_markdown(hjust = .5, size = 22, face = "bold"),
        plot.subtitle = ggtext::element_markdown(hjust = .5, size = 16, face = "italic"),
        axis.title = ggtext::element_markdown(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill= "#f8f8f8",color = "#f8f8f8"),
        legend.position = "none",
        plot.caption = element_text(size = 12))


ggsave("utah_sr_2021.png",width = 10,height = 8)

# For WIP animation
library(magick)
## list file names and read in
imgs <-  c(paste0("Rplot",str_pad(0:24,width = 2,pad = "0"),".png"),rep("Rplot24.png",10))
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "sr_wip.gif")

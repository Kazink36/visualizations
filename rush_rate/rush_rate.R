library(cfbplotR)
library(tidyverse)
library(cfbfastR)

pbp <- load_cfb_pbp(2018:2021)
alt_color_teams <- c("LSU","Tennessee")

pbp %>%
  filter(pos_team == "Florida",down <= 2) %>%
  filter(def_pos_team %in% valid_team_names()) %>%
  group_by(def_pos_team,year) %>%
  summarize(rush_rate = sum(rush)/(sum(rush)+sum(pass))) %>%
  ungroup() %>%
  ggplot(aes(x = rush_rate,y = def_pos_team)) +
  geom_col(aes(fill = def_pos_team,color = def_pos_team)) +
  geom_vline(xintercept = .5, linetype = "dashed",color = "black") +
  geom_cfb_logos(aes(team = def_pos_team, x = .05),height = .09) +
  geom_text(data = tibble(x = .75, y = "Tennessee", year = 2019),
            aes(x=x,y = y),
            label = "Equal # of\nPasses and Rushes") +
  geom_curve(data = tibble(x = .75,xend = .5, y = "South Carolina", yend = "Missouri",year = 2019),
             aes(x = x, xend = xend,y = y, yend = yend),
             arrow = arrow(angle = 15,length = unit(.15, "in"),type = "closed"),
             angle = 90,
             curvature = -.5) +
  facet_wrap(~year, scales = "free_y") +
  labs(x = "% of early down plays that were runs", y = "Opponent",title = "Florida's Early Down Rushing Rate by Year",caption = "Figure: @JaredDLee | Inspired by @Bfarrell727\nData: @cfbfastR") +
  scale_fill_cfb(alt_colors = alt_color_teams) +
  scale_color_cfb(alt_colors = valid_team_names()[-which(valid_team_names() %in% alt_color_teams)]) +
  scale_x_continuous(limits = c(0,1),breaks = seq(0,1,.2),labels = scales::percent) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold",size = 18,hjust = .5))

ggsave("rush_rate.png",width = 8, height = 8)





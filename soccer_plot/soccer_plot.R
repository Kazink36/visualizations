#params
team_home<-"BYU"
team_away <- "Utah"
nudge_y <- .35
width <- .25
gap <- .025
text_size <- 6
text_color <- "gray30"
game_state <- tibble(team_home = team_home,team_away = team_away,
                     score_home = 0,score_away = 1,
                     clock_time = "77:29",half = "2nd")

data <- tibble(team = c(team_home,team_away),
               posession = c(.75,.25),
               goal_att = c(23,4),
               shot_on_goal = c(6,1),
               shot_off_goal = c(12,1),
               block_shot = c(5,2),
               free_kick = c(10,9)
)

dict <- tibble(var = c("posession","goal_att","shot_on_goal","shot_off_goal","block_shot","free_kick"),
               label = c("Ball Posession","Goal Attempts","Shots On Goal","Shots Off Goal","Blocked Shots","Free Kicks"))


# team_info from cfb_team_info in cfbscrapR
colors <- team_info$color
names(colors) <- team_info$school
teams <- team_info %>% filter(school %in% c(team_home,team_away)) %>% select(school,mascot,logo,color)
plot_data <- data %>%
  mutate(across(where(is.numeric),function(x){x/sum(x)},.names = "{.col}_prop")) %>%
  pivot_longer(2:length(.)) %>%
  mutate(prop = str_detect(name,"_prop"),
         name = str_remove(name,"_prop"),
         value = if_else(prop & team == team_home,-1*value,value)
  ) %>%
  left_join(dict,by = c("name"="var")) %>%
  mutate(name = fct_rev(fct_relevel(name,dict$var)))
ggplot() +

  geom_col(data = plot_data %>% filter(prop),
           aes(y = name, x = 1),
           fill = "gray70",
           width = width,
           alpha = .6,
           position = position_nudge(x = gap/2)) +

  geom_col(data = plot_data %>% filter(prop),
           aes(y = name, x = -1),
           fill = "gray70",
           width = width,
           alpha = .6,
           position = position_nudge(x = -gap/2)) +
  geom_col(data = plot_data %>% filter(prop,team == team_home),
           aes(y = name, x = value,fill = team),
           width = width,
           position = position_nudge(x = -gap/2)) +
  geom_col(data = plot_data %>% filter(prop,team == team_away),
           aes(y = name, x = value,fill = team),
           width = width,
           position = position_nudge(x = gap/2)) +
  geom_text(data = plot_data %>% filter(prop),
            aes(y = name,x = 0,label = label),
            nudge_y = nudge_y,
            size = text_size,
            color = text_color)+
  geom_text(data = plot_data %>% filter(!prop) %>%
              mutate(value = ifelse(value < 1,glue::glue("{value*100}%"),value)),
            aes(y = name, x = if_else(team == team_home,-1,1), label = value,
                hjust = if_else(team == team_home,0,1)),
            nudge_y = nudge_y,
            size = text_size,
            color = text_color,
  ) +
  geom_segment(aes(x = -1.1,xend = 1.1,y = 6.75,yend = 6.75),color = "darkgreen",size = 1) +
  annotate("text",x = 0,y = 7.5,
           label = glue::glue("{game_state$score_home} - {game_state$score_away} \n {game_state$half} Half - {game_state$clock_time}"),
           size = text_size,
           color = text_color) +
  ggimage::geom_image(data = teams,
                      aes(image = logo,y = 7.5,x = ifelse(school == team_home,-.7,.7)),
                      size = .125) +
  scale_fill_manual(values = colors[c(team_home,team_away)],guide = "none") +
  expand_limits(y = 8) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f7f7f7",color = "#f7f7f7"))
ggsave("output/soccer_plot.png",width = 6,height = 6,units = "in")


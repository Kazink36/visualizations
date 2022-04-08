#https://twitter.com/CFBNumbers/status/1512474661606539264
library(cfbfastR)
library(cfbplotR)
library(tidyverse)

draft_picks <- map_df(1967:2021,cfbd_draft_picks)

plot_data <- draft_picks %>%
  filter(nfl_team == "Miami") %>%
  count(college_team) %>%
  mutate(college_team_fct = fct_rev(fct_reorder(college_team,n)))%>%
  filter(college_team %in% valid_team_names("FBS")) %>%
  arrange(college_team_fct) %>%
  mutate(angle = (n()-row_number())/n()*360+90)



nfl_teams <- cfbd_draft_teams()

plot_data %>%
  ggplot(aes(x = college_team_fct,y = n)) +
  geom_col(aes(color = college_team),width = 0.6,fill = "#454748",size = 1) +
  geom_text(aes(label = n),color = "white",nudge_y = -.1, size = 2) +
  #geom_text(aes(y = n+1,label = college_team)) +
  geom_cfb_logos(aes(y = n + 1, team = college_team, angle = angle), height = 0.02, alpha = .8) +
  annotate(GeomFromPath, x = 0, y = -8, path = nfl_teams %>% filter(nfl_location == "Miami") %>% pull(nfl_logo),height = 0.25) +
  annotate(GeomText,x = 0, y = -1.5 ,label = "NFL Draft Picks",size = 5.5,fontface = "bold")+
  annotate(GeomText,x = 0, y = -2.15 ,label = "Super Bowl Era", size = 3.5,fontface = "bold")+
  annotate(GeomText,x = 0, y = -3 ,label = "Figure: @CFBNumbers | Data: @CFB_Data with @cfbfastR",size = 2,fontface = "bold") +
  expand_limits(y = -8) +
  scale_color_cfb()+
  coord_polar(direction = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#ececec",color = "#ececec"))


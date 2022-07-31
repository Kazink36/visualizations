library(cfbfastR)
library(cfbplotR)
library(tidyverse)

pressure <- read_csv("passing_pressure.csv") #PFF

plot_data <- pressure %>%
  filter(position == "QB") %>%
  group_by(team_name) %>%
  filter(player_game_count == max(player_game_count)) %>%
  ungroup() %>%
  select(player,team_name,blitz_grades_offense,no_blitz_grades_offense) %>%
  mutate(team = clean_school_names(tolower(team_name),keep_non_matches = FALSE),
         team2 = clean_school_names(team_name,keep_non_matches = FALSE)) %>%
  mutate(team = ifelse(is.na(team),team2,team)) %>%
  #filter(is.na(team)) %>%
  # Attempted to some cleaning of team names for cfbplotR, but didn't finish
  mutate(team2 = case_when(team_name == "MISS STATE" ~ "Mississippi State",
                           team_name == "W KENTUCKY" ~ "Western Kentucky",
                           team_name == "NEW MEX ST" ~ "New Mexico State",
                           team_name == "FRESNO ST" ~ "Fresno State",
                           team_name == "WAKE" ~ "Wake Forest",
                           team_name == "E CAROLINA" ~ "East Carolina",
                           team_name == "NC STATE" ~ "NC State",
                           team_name == "BOWL GREEN" ~ "Bowling Green",
                           team_name == "COLO STATE" ~ "Colorado State",
                           team_name == "BOISE ST" ~ "Boise State",
                           team_name == "W VIRGINIA" ~ "State",
                           team_name == "E MICHIGAN" ~ "State",
                           team_name == "N CAROLINA" ~ "State",
                           team_name == "W MICHIGAN" ~ "State",
                           team_name == "CAL" ~ "State",
                           team_name == "UTAH ST" ~ "State",
                           team_name == "BALL ST" ~ "State",
                           team_name == "FAU" ~ "State",
                           team_name == "ARK STATE" ~ "State",
                           team_name == "LA LAFAYET" ~ "State",
                           team_name == "MICH STATE" ~ "State",
                           team_name == "WASH STATE" ~ "State",
                           team_name == "APP STATE" ~ "State",
                           team_name == "MIAMI FL" ~ "State",
                           team_name == "TEXAS A&M" ~ "State",
                           team_name == "ARIZONA ST" ~ "State",
                           team_name == "S ALABAMA" ~ "State",
                           team_name == "OKLA STATE" ~ "State",
                           team_name == "VA TECH" ~ "State",
                           team_name == "OREGON ST" ~ "State",
                           team_name == "USF" ~ "State",
                           team_name == "N ILLINOIS" ~ "State",
                           team_name == "MIAMI OH" ~ "State",
                           team_name == "C MICHIGAN" ~ "State",
                           team_name == "TEXAS ST" ~ "State",
                           team_name == "N TEXAS" ~ "State",
                           team_name == "LA TECH" ~ "State",
                           team_name == "FLORIDA ST" ~ "State",
                           team_name == "UCONN" ~ "State",
                           team_name == "COAST CAR" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           team_name == "STATE" ~ "State",
                           TRUE ~ NA_character_
                           ))
plot_data %>%
  ggplot() +
  geom_abline(slope = 1,intercept = 0,alpha = 0.5) +
  geom_point(aes(x = no_blitz_grades_offense,y = blitz_grades_offense,color = no_blitz_grades_offense>blitz_grades_offense),size = 2, alpha = 0.8) +
  geom_text(data = tribble(
    ~"text", ~"x", ~"y",~"color",
    "Better Facing Blitz",50,90,FALSE,
    "Better Facing No Blitz", 75,40,TRUE
  ), mapping = aes(label = text,x = x, y = y,color = color),size = 6,family = "Oswald",fontface = "bold") +
  geom_text(data = plot_data %>% filter(player == "Cameron Rising" | player == "Caleb Williams"),
            aes(x = no_blitz_grades_offense,y = blitz_grades_offense-5, label = player),family = "Oswald",fontface = "bold") +
  geom_cfb_logos(data = plot_data %>% filter(player == "Cameron Rising" | player == "Caleb Williams"),
            aes(x = no_blitz_grades_offense,y = blitz_grades_offense, team = team),height = 0.06) +
  labs(x = "No Blitz Grade", y = "Blitz Grade",caption = "@JaredDLee | Data: PFF",
       title = "PFF QB Grades When Facing Blitz",
       subtitle = "2021 Season") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(40,100,10)) +
  scale_y_continuous(breaks = seq(40,100,10)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        title = element_text( size = 16,family = "Oswald"),
        text = element_text(size = 16,family = "Oswald"),
        plot.title = element_text(hjust = .5, face = "bold", size = 24,family = "Oswald"),
        plot.caption = element_text(size = 13),
        plot.background = element_rect(fill = "#f8f8f8")
        )
ggsave("blitz_qb.png",width = 8,height = 8)

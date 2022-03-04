library(tidyverse)
library(rvest)
library(jsonlite)
library(rnaturalearth)
library(ggtext)
library(glue)

url <- "https://data2.unhcr.org/population/get/sublocation?widget_id=283559&sv_id=54&population_group=5459,5460&forcesublocation=0&fromDate=1900-01-01"
# url2 <- "https://data2.unhcr.org/population/get/sublocation?geo_id=0&forcesublocation=1&widget_id=283557&sv_id=54&color=%233c8dbc&color2=%23303030&population_group=5460"
raw <- read_html(url)
data <- raw %>%
  html_text() %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  .$data

data <- data %>%
  select(geomaster_name:individuals) %>%
  select(-population_groups_concat) %>%
  mutate(region = case_when(geomaster_name == "Republic of Moldova" ~ "Moldova",
                            geomaster_name == "Russian Federation" ~ "Russia",
                            TRUE ~ geomaster_name),
         centroid_lon = as.numeric(centroid_lon),
         centroid_lat = as.numeric(centroid_lat)) %>%
  as_tibble()

map <- ggplot2::map_data("world")

map <- map %>% mutate(color = case_when(region %in% data$region ~ "#ece9e0",
                                        region == "Ukraine" ~ "#c8d6e5",
                                        TRUE ~ "#f5f4ee"))
country_labels <- data %>%
  mutate(individuals = as.numeric(individuals),
         label = glue::glue("<span style='color:#232c71'>**{region}**<br>{prettyNum(individuals,big.mark = ',')}</span>"),
         centroid_lon = case_when(region == "Other European countries" ~ 21,
                                  region == "Slovakia" ~ centroid_lon + 1.25,
                                  region == "Hungary" ~ centroid_lon + 1.25,
                                  TRUE ~ centroid_lon),
         centroid_lat = case_when(region == "Other European countries" ~ 45,
                                  region == "Romania" ~ centroid_lat + 1,
                                  TRUE ~ centroid_lat),
         nudge_x = case_when(region == "Other European countries" ~ -3.1,
                             region == "Russia" ~ 1.25,
                             region == "Belarus" ~ -1,
                             region == "Hungary" ~ -3,
                             region == "Slovakia" ~ -2.75,
                             region == "Poland" ~ -.25,
                             region == "Romania" ~ -.25,
                             region == "Moldova" ~ -.25,
                             TRUE ~ 0),
         nudge_y = case_when(region == "Romania" ~ -.85,
                             region == "Moldova" ~ -.95,
                             region == "Belarus" ~ .25,
                             TRUE ~ 0),
         text_x = centroid_lon + nudge_x,
         text_y = centroid_lat + nudge_y)


people <- tribble(
  ~ x, ~ y,
  # Poland
  22, 50.6,
  22.25, 50.8,
  22.5, 51,
  22.3,50.4,
  22.55,50.6,
  22.65,50.3,
  22.85,50.45,
  23,50.15,
  # Belarus
  28,53.1,
  28.1,52.7,
  27.9,52.3,
  28.05,51.9,
  # Russia
  38,51,
  37.8,50.55,
  37.7,50.9,
  37.4,50.5,
  # Slovakia
  22.2,49,
  22,48.85,
  21.75,48.95,
  21.55,48.8,
  # Hungary
  21.75,47.5,
  21.55,47.75,
  21.85,47.8,
  22.1,47.75,
  22.4,47.9,
  22.15,48.1,
  # Moldova
  28,48.1,
  28.2,47.95,
  27.9,47.75,
  # Romania
  25.25,47.65,
  25.05,47.5,
  25.35,47.35,
  # Other
  23.75,47.75,
  23.5,47.6,
  23.3,47.35,
  23.05,47.1,
  22.9, 46.8,
  22.6, 46.6,
  22.5, 46.2,
  22.25,45.9,
  22,45.7,
  21.8,45.6,
  21.6,45.4
) %>%
  mutate(label = "B")

ggplot(map,aes(long,lat)) +
  geom_polygon(aes(fill = color,group = group),color = "#b0b0a8") +
  geom_text(data = people,aes(x = x, y = y, label = label),family = "WeePeople", size = 9, color = "#90acca") +
  geom_point(data = country_labels, aes(x = centroid_lon, y = centroid_lat, size = individuals),shape = 21, fill = "#c8d6e5",color = "#436080") +
  geom_textbox(data = country_labels, aes(x = text_x, y = text_y, label = label),
                fill = NA,width = .05,color = NA, size = 7, hjust = 0.5) +
  annotate(GeomText,x = 31.23369, y = 49, label = "U K R A I N E", size = 8, fontface = "bold", hjust = 0.5, color = "#232c71") +
  coord_quickmap(xlim = c(17,42), ylim = c(44,55)) +
  scale_fill_identity() +
  scale_radius(range = c(8,100)) +
  labs(title = "Mass Exodus",
          subtitle = "Refugee arrivals from Ukraine, Feb 24th - Mar 3rd 2022",
       caption = "Source: UNHCR") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 24,face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16, color = "grey50",hjust = 0),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("test.png", width = 16, height = 10)

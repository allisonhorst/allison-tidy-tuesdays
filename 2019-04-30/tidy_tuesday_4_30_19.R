# Tidy Tuesday 4/30/19
# Allison Horst
# Bird collisions in Chicago

# Goals:
# Try circle packing?

#----------------
# Get data:

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

# mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

#----------------
# Get packages:

library(tidyverse)
library(lubridate)
library(ggmosaic)
library(packcircles)
library(ggrepel)

#----------------
# Some wrangling for exploration:

# Total collision counts by locality, flight_call, habitat, stratum:
bird_sum <- bird_collisions %>%
  filter(flight_call != "Rare") %>%
  group_by(locality, flight_call, habitat, stratum) %>%
  tally()

# Counts of collisions by year (Chicago):
bird_year <- bird_collisions %>%
  mutate(year = year(date)) %>%
  filter(locality == "CHI") %>%
  group_by(year, family, flight_call) %>%
  tally()

# Counts of collisions by family:
bird_tot <- bird_collisions %>%
  group_by(flight_call, family) %>%
  tally()

#----------------
# Some exploratory graphs:

# Plot collisions over years:
ggplot(bird_year, aes(x = year, y = n)) +
  geom_point(aes(color = family,
                 pch = flight_call))

# Mosaic plot by flight call (works, but not interesting)
ggplot(bird_sum) +
  geom_mosaic(aes(weight = n,
                  x = product(locality, habitat),
                  fill = flight_call))

#----------------
# Circle packing try...

# Make circles!

circles <- packcircles::circleProgressiveLayout(bird_tot$n, sizetype='area')

data <- data.frame(bird_tot, circles) %>%
  mutate(id = row_number())

data_vertices <- circleLayoutVertices(circles, npoints=100)
data_join <- full_join(data, data_vertices)
data_min_join <- left_join(data, data_vertices)

# Create final circle graph:

ggplot() +
  geom_polygon(data = data_join,
               aes(x, y,
                   group = id,
                   fill = factor(flight_call)),
               color = "NA") +
   geom_polygon(data = data_vertices,
               aes(x, y, group = id),
               size = 0.5,
               fill = NA,
               color = NA) +
  scale_fill_manual(values = c("darkorange","cyan4","slateblue1"),
                    breaks = c("No","Rare", "Yes"),
                    name = "Flight call?") +
  geom_text_repel(data = data_min_join,
                  aes(x, y, label = family, size = radius),
                  segment.size = 0.2,
                  min.segment.length = 0.4,
                  segment.color = "black",
                  color = "black",
                  force = 35,
                  family = "Arial",
                  fontface = "italic"
                  ) +
  scale_radius(range = c(2,8), guide = "none") +
  labs(x = "", y = "",
       title = "Bird collisions in Chicago by family",
       subtitle = "Circle areas ~ Number of collisions") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 12),
        text = element_text(family = "Arial")
        ) +
  coord_equal()

ggsave("2019-04-30/bird_collision_circles.png")


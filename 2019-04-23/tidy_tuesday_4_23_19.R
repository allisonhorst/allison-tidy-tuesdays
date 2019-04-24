# Tidy Tuesday 4/23/19
# Allison Horst
# Anime!

# Goals this week: fun!

#######
# Load packages
#######

library(tidyverse)
library(ggdark)
library(extrafont)

#######
# Get data
#######

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

########
# Go exploring
########

# Simplify with all shows:

all_shows <- tidy_anime %>%
  select(-synopsis, - background) %>%
  distinct(name, .keep_all = TRUE)

# Find and keep shows from the top 10 most common genres (by # of shows in genre):
top_genres <- tidy_anime %>%
  select(-synopsis, -background) %>%
  filter(genre != "NA") %>%
  group_by(genre) %>%
  tally() %>%
  arrange(-n) %>%
  head(6) %>%
  inner_join(tidy_anime)

# No real difference in scores for top genres:
ggplot(top_genres, aes(x = score)) +
  geom_density(aes(fill = genre)) +
  facet_wrap(~genre)

# Change in scores over time?
ggplot(top_genres, aes(x = start_date, y = score)) +
  geom_point() +
  geom_line(aes(color = genre))

# Cumulative sum over time?
time_df <- top_genres %>%
  mutate(show = 1) %>%
  arrange(genre, start_date) %>%
  select(genre, name, start_date, show) %>%
  group_by(genre) %>%
  mutate(totes = cumsum(show))

ggplot(time_df, aes(x = start_date, y = totes)) +
  geom_line(aes(color = genre))

# OK, actually that wasn't that exciting. I'll try something else.

ggplot(top_genres, aes(x = score, y = popularity)) +
  geom_point(aes(color = genre), alpha = 0.5) +
  facet_wrap(~genre) + # This is kind of weird
  theme_dark() +
  scale_color_manual(values = c("red","orange","yellow","purple","black","white"))

# Make function for nice scientific notation (help from: https://stackoverflow.com/questions/10762287/how-can-i-format-axis-labels-with-exponents-with-ggplot2-and-scales/45867076)

scientific_10 <- function(x) {
  parse(text=gsub("e", "%*% 10^", scales::scientific_format()(x)))
}

# Then a graph of score vs. popularity:

ggplot(all_shows, aes(x = score, y = popularity)) +
  geom_hex(bins = 50,
           binwidth = c(0.2, 400)) +
  scale_fill_gradientn(colors = c("slateblue4",
                                  "brown1",
                                  "orange",
                                  "yellow",
                                  "white"),
                       name = "Number of shows:") +
  guides(fill = guide_colourbar(ticks = TRUE,
                                barwidth = 20,
                                barheight = 0.5,
                                direction = "horizontal",
                                title.position = "top")) +
  scale_y_continuous(expand = c(0,0), label = scientific_10) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,10),
                     breaks = seq(0,10, by = 2)) +
  labs(x = "Score (higher = better)",
       y = "Popularity\n(members with show in MyAnimeList)",
       title = "Anime: higher scores don't = greater popularity") +
  ggdark::dark_theme_bw() +
  theme(text = element_text(family = "Andale Mono"),
       # panel.spacing.x = unit(1.0, "lines"),
       # panel.spacing.y = unit(1.0, "lines"),
        legend.position = "bottom"
  )

ggsave("anime.png", width = 8, height = 7)

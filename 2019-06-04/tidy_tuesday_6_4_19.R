#################
# Tidy Tuesday 6/4/2019
# Allison Horst
# Ramen ratings!

# "This week's dataset is a ramen ratings dataset from The Ramen Rater. H/t to Data is Plural."
##################

# Attach packages
library(tidyverse)
library(LaCroixColoR)
library(extrafont)
library(ggbeeswarm)
library(ggridges)

# Get the data
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

# Find the most commonly rated brands:
ramen_common <- ramen_ratings %>%
  group_by(brand) %>%
  tally() %>%
  arrange(-n) %>%
  head(20) %>%
  inner_join(ramen_ratings)

# Brand medians:
brand_medians <- ramen_common %>%
  group_by(brand) %>%
  summarize(
    medians = median(stars, na.rm = TRUE)
  ) %>%
  arrange(-medians)

# Relevel brand factor levels by median
ramen_common$brand <- fct_relevel(ramen_common$brand, levels = brand_medians$brand)

# Palette specs
pal <- lacroix_palette("Berry", n = 20, type = "continuous")

# Only keep those brands from the original df, plot
ramen_top_brands <- ramen_common %>%
  filter(brand %in% unique(ramen_common$brand))

ggplot(ramen_top_brands, aes(x = reorder(brand, desc(brand)), y = stars)) +
  # geom_quasirandom(aes(color = brand),
  #              alpha = 0.3,
  #              size = 1) +
  geom_jitter(size = 1,
              alpha = 0.3,
              aes(color = brand),
              width = 0.1) +
  geom_boxplot(size = 0.2,
               aes(fill = brand),
               alpha = 0.8,
               outlier.alpha = 0) +
  geom_point(data = brand_medians, aes(x = brand, y = medians),
             color = "gray10",
             fill = "gray10",
             size = 2,
             pch = 21) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Josefin Sans"),
        legend.position = "NA",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        plot.subtitle = element_text(color = "gray50"),
        plot.caption = element_text(color = "gray50")) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(x = "Ramen Brand",
       y = "Rating (5 = best)\n",
       title = "Ramen ratings by brand*",
       subtitle = "Data from The Ramen Rater",
       caption =
         "*for 20 brands with most observations (n) in dataset")

ggsave("2019-06-04/ramen_by_brand.png", width = 6, height = 6 )

# Just more messing around...

pal_2 <- pal <- lacroix_palette("Apricot", n = 5, type = "continuous")

ramen_ratings %>%
  filter(brand %in% unique(ramen_common$brand)) %>%
  ggplot(., aes(brand, stars)) +
  geom_quasirandom(aes(color = brand), alpha = 0.5, size = 2) +
  coord_flip() +
  scale_color_manual(values = pal) +
  theme_light() +
  theme(
    text = element_text(family = "Muli"),
    legend.position = "NA"
  )

# Hm cool.
# Now investigating by country:

# Top 20 countries with most ratings
common_countries <- ramen_ratings %>%
  group_by(country) %>%
  tally() %>%
  arrange(-n) %>%
  head(20) %>%
  inner_join(ramen_ratings) # Join back. Cool.

# Find median order for factor releveling:
country_medians <- common_countries %>%
  group_by(country) %>%
  summarize(
    medians = median(stars, na.rm = TRUE)
  ) %>%
  arrange(-medians)

# Use that order to relevel the country factor levels in common_countries
common_countries$country <- fct_relevel(common_countries$country, levels = country_medians$country)


# Plot ramen by country

pal_3 <- lacroix_palette("Berry", n = 20, type = "continuous")


ggplot(common_countries, aes(x = reorder(country, desc(country)), y = stars)) +
  geom_quasirandom(alpha = 0.3,
                   aes(color = country)) +
  geom_boxplot(alpha = 0.6,
               size = 0.2,
               color = "black",
               aes(fill = country),
               outlier.color = NA) +
  geom_point(data = country_medians, aes(x = country, y = medians),
             color = "gray20",
             size = 3,
             pch = 19) + #124 is vertical line
  scale_y_continuous() +
  scale_fill_manual(values = pal_3) +
  scale_color_manual(values = pal_3) +
  theme_minimal() +
  coord_flip() +
  theme(
    legend.position = "NA",
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "Josefin Sans")
  ) +
  labs(y = "Rating (5 = better)",
       x = "Country Produced",
       title = "Ramen ratings by production country*",
       subtitle = "Data from The Ramen Rater")

ggsave("2019-06-04/ramen.png", width = 5, height = 5)


# Switch side of axis labels (y)
# Add (n = #) to each row for number of observations
# Add caption with *Only 20 countries with highest # observations included

####################
# FINAL GGRIDGES GRAPH
####################
# Some other weird tests of things
# ggridges?
# Using the ramen_top_brands dataset

pal_4 <- lacroix_palette("Coconut", n = 20, type = "continuous")

ggplot(ramen_top_brands, aes(x = stars, y = brand)) +
  geom_density_ridges(scale = 7,
                      aes(fill = brand),
                      size = 0.3,
                      color = "NA") +
  scale_fill_manual(values = pal_4) +
  scale_color_manual(values = pal_4) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  theme_minimal() +
  theme(text = element_text(family = "Carrois Gothic"),
        legend.position = "NA",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(color = "gray50"),
        plot.caption = element_text(color = "gray50"),
        plot.title = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12)
        ) +
  labs(x = "Rating (5 = best)\n",
       y = "Ramen Brand\n",
       title = "Ramen ratings by brand*",
       subtitle = "Data from The Ramen Rater",
       caption =
         "*for 20 brands with most observations (n) in dataset")

ggsave("2019-06-04/ramen_ggridges.png", width = 7, height = 7)

#######
# Tidy Tuesday 5/28/2019
# Wine ratings
#######

# Attach packages
library(tidyverse)
library(extrafont)
library(ggdark)

# Get the data:
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

# A bunch of wrangling (much unnecessary) + exploration station:

# Find the points-to-price ratios:
wine_deal <- wine_ratings %>%
  select(points, price, title) %>% # Only keep these columns
  mutate(pp_ratio = points/price) %>% # Find the points:price ratio
  arrange(-pp_ratio) # Arrange by high-to-low ratio

# Checking counts for each wine type (don't really care):
wine_counts <- wine_ratings %>%
  group_by(title) %>%
  tally() %>%
  arrange(-n)

# Find the top 15 countries with them most reviews:
country_counts <- wine_ratings %>%
  group_by(country) %>%
  tally() %>%
  arrange(-n) %>%
  head(15) %>%
  select(country)

# Find the median point:price ratio for those 15 countries:
# Note: something is effed here. (not reproducible right now)
country_medians <- wine_deal %>%
  inner_join(wine_ratings) %>%
  inner_join(country_counts) %>%
  group_by(country) %>%
  summarize(
    med_ratio = median(pp_ratio, na.rm = TRUE)
  ) %>%
  arrange(-med_ratio)

# Join to have prices, number, ratio in single table, relevel by medians:
wine_all <- wine_deal %>%
  inner_join(wine_ratings) %>%
  inner_join(wine_counts) %>%
  inner_join(country_counts) %>%
  select(title, pp_ratio, country, variety,n) %>%
  drop_na(country) %>%
  mutate(country = as.factor(country)) %>% # Not necessary?
  mutate(country = fct_relevel(country, country_medians$country))

# Violin plot of points:price ratios by country
ggplot(wine_all, aes(x = reorder(country, desc(country)), y = pp_ratio)) +
  geom_violin(aes(color = country, fill = country), width = 1.0) +
  geom_boxplot(fill = NA, color = "white", width = 0.4, size = 0.3, outlier.color = NA) +
  labs(x = "Country\n",y = "\nPoints-per-price ratio (higher = better)", title = "Wine points:price ratio (sweet deal metric) by country", subtitle = "*For the 15 countries with the highest number of reviews in Kaggle dataset") +
  dark_mode(theme_minimal()) +
  theme(legend.position = "NA",
        text = element_text(family = "Muli"),
        plot.subtitle = element_text(size = 8, face = "italic")) +
  coord_flip()

ggsave("2019-05-28/wine_deals.png", width = 8, height = 7)

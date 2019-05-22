# Tidy Tuesday 5/19/2019
# Global plastics
# Allison Horst

# Attache packages:
library(tidyverse)
library(janitor)
library(extrafont)
library(treemapify)

# Get the data:
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>% clean_names()

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv") %>% clean_names()

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv") %>% clean_names()

# Make df names and column names more manageable:
coastal <- coast_vs_waste %>%
  rename(mis_plastic = mismanaged_plastic_waste_tonnes,
         coast_pop = coastal_population,
         tot_pop_gm = total_population_gapminder)

mis_gdp <- mismanaged_vs_gdp %>%
  rename(mis_plastic_percap = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
         gdp_per_cap = gdp_per_capita_ppp_constant_2011_international_rate,
         tot_pop_gm = total_population_gapminder)

waste_gdp <- waste_vs_gdp %>%
  rename(plast_waste_percap = per_capita_plastic_waste_kilograms_per_person_per_day,
         gdp_per_cap = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
         tot_pop_gm = total_population_gapminder)

# Join them together (only includes 2010 data when NAs removed...)
# NEED TO DOUBLE CHECK THESE JOINS, UNITS & CONVERSIONS...
all_join <- full_join(coastal, mis_gdp) %>% # join
  full_join(waste_gdp) %>% # again
  drop_na(mis_plastic) %>% # get rid of NAs (only keeps 2010)
  filter(entity != "World") %>% # No world total
  mutate(tot_plastic_2010 = tot_pop_gm*plast_waste_percap*365) %>% # calc totals (ANNUAL KG)
  mutate(mis_plastic_kg = mis_plastic*907.185) %>%  # Convert from tons to kg (1 ton = 907.185 kg)
  mutate(perc_mismanaged = mis_plastic_kg/tot_plastic_2010) %>%
  arrange(-tot_plastic_2010)

# Coastal pop vs. mismanaged plastics
ggplot(all_join, aes(x = coast_pop, y = mis_plastic)) +
  geom_point()

ggplot(all_join, aes(x = gdp_per_cap, y = perc_mismanaged)) +
  geom_point()

# Test log (base-10) scale?
ggplot(all_join, aes(x = log10(coast_pop), y = log10(mis_plastic))) +
  geom_point() # Eh. I think log scales are hard to think about.

# Treemap?

ggplot(all_join, aes(area = tot_plastic_2010, label = entity, fill = tot_plastic_2010)) +
  geom_treemap(color = "white", start = "topleft") +
  geom_treemap_text(min.size = 4, place = "center", family = "Carrois Gothic", color = "white", start = "topleft") +
  scale_fill_gradientn(colors = c("black","green3")) +
  theme(legend.position = "NA") +
  labs(title = "Total plastics, 2010") +
  theme(text = element_text(family = "Carrois Gothic"))

ggsave("2019-05-20/plastic_treemap.png", width = 8, height = 8)

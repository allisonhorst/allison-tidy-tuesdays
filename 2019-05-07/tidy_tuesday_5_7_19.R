# Tidy Tuesday 5/7/2019
# Student:teacher class size ratios (global)

# Attach packages
library(tidyverse)
library(janitor)
library(sf)

# Get data:

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# Averaged across all indicators (levels) and years...

students_all <- student_ratio %>%
  group_by(country_code) %>%
  summarize(
      mean_ratio = mean(student_ratio, na.rm = TRUE)
    ) %>%
  ungroup() %>%
  rename(iso3 = country_code)

# Get global map data, join to student_2016 data:

globe <- st_read(dsn = "2019-05-07", layer = "TM_WORLD_BORDERS_SIMPL-0.3") %>%
  st_transform(4326) %>%
  clean_names() %>%
  full_join(students_all)

# Plot a single map...

ggplot() +
  geom_sf(data = globe,
          aes(fill = mean_ratio),
          color = "white",
          size = 0.1
          ) +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colors = c("royalblue1","magenta","orange","gold"),
                       name = "Average student-teacher ratio") +
  labs(title = "#tidytuesday: student-teacher ratios\n(average across all years & levels in dataset)") +
  theme_void() +
  theme(legend.position = c(0.2, 0.35), legend.direction = "vertical",
        plot.background = element_rect(fill = "gray10", color = NA),
        panel.background = element_rect(fill = "gray10", color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = "gray10", colour = NA),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 32)
  )

# plot.margin = margin(1, 1, 1, 1, "cm")

ggsave("images/student_ratios_map.png", width = 8, height = 5)

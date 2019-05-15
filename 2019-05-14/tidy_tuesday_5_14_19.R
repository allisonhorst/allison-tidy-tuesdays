# Nobel Prize Winners
# #tidytuesday 5/14/2019

# Allison Horst

# Goal: Create a timeline of women who've won the Nobel Prize

# Get data on Nobel Prize winners (more information here: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14):

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

# Load packages:

library(tidyverse)
library(lubridate)
library(extrafont)
library(showtext)

font_add_google(name = "Quicksand", family = "quicksand")

# Some wrangling

nobels <- nobel_winners %>%
  mutate(yrand = rnorm(969, mean = 0, sd = 1)) %>%
  filter(laureate_type == "Individual")


# Create random sequences for year jitter:

set.seed(1001)
r_seq <- rnorm(48, mean = 0, sd = 10)

set.seed(1002)
r_seq_2 <- rnorm(939, mean = 0, sd = 0)


# Add columns with jittered year (using sequences above)

women_nobels <- nobel_winners %>%
  filter(laureate_type == "Individual") %>%
  filter(gender == "Female") %>%
  mutate(f_yrand = 1) %>%
  mutate(year_jitter = prize_year + r_seq)

all_nobels <- nobels %>%
  mutate(all_rand = 1) %>%
  mutate(year_jitter_all = prize_year + r_seq_2) %>%
  mutate(gender = fct_relevel(gender, "Male", "Female")) %>%
  mutate(category = fct_relevel(category, "Physics","Economics","Chemistry","Medicine","Literature","Peace"))

# Make text
text_df <- data.frame(
  label = c("Physics: 2/222","Economics: 2/83","Chemistry: 4/194", "Medicine: 12/227", "Literature: 14/113", "Peace: 14/100"),
  category = c("Physics","Economics","Chemistry","Medicine","Literature","Peace"),
  x = c(1905,1905,1905),
  y = c(1,2,3)
)

# Chemistry, Economics, Literature, Medicine, Peace, Physics

# women_nobels$rand_val <- r_seq
#
# women_nobels_jitteryear <- women_nobels %>%
# mutate(jitter_year = prize_year + rand_val)

# Physics: 2/222
#

# Trying geom_linerange

ggplot(women_nobels, aes(y = f_yrand, ymin = 0, x = year_jitter, ymax = f_yrand)) +
  geom_linerange(size = 1) +
  geom_text(aes(label = full_name),
            angle = 50,
            vjust = 0,
            hjust = -0.05,
            size = 2)

# This all looks hideous and the y-axis doesn't make sense.

# Trying with all nobel winners:

ggplot(all_nobels, aes(y = all_rand,
                       ymin = 0,
                       x = year_jitter_all,
                       ymax = all_rand)) +
  geom_linerange(size = 1.5,
                 aes(color = gender),
                 alpha = 0.4) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,1), limits = c(1901, 2016), breaks = c(1901,2016)) +
  scale_color_manual(values = c("mediumorchid4","cyan")) +
  facet_wrap(~category, ncol = 1, strip.position = "left") +
  theme_minimal() +
  labs(title = "Individual Nobel Prize Winners\n1901 - 2016") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "gray60", size = 10),
        strip.text = element_text(color = "gray60", size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "gray70"),
        plot.margin=unit(c(1,1,1,1.2),"cm"),
        plot.title = element_text(color = "gray60"),
        text = element_text(family = "Times New Roman")
  )

ggsave("2019-05-14/nobel_winners.png", width = 7, height = 7)

# Get some summary counts:

nobel_sum <- nobel_winners %>%
  filter(laureate_type == "Individual") %>%
  group_by(category, gender) %>%
  tally()

# Physics: 2/222
# Economics: 2/83
# Chemistry: 4/194
# Medicine: 12/227
# Literature: 14/113
# Peace: 14/100



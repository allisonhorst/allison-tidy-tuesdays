# Tidy Tuesday 4/16/19
# Allison Horst
# Take a decent graph, and make it an absolute abomination


#######
# Load packages
#######

library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(ggpubr)
library(forecast)
library(lubridate)
library(ggdark)

#######
# Get data
#######

brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

corbyn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv")

#dogs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv")

#eu_balance <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")

#pensions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")

#trade <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")

#women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

#########
# Corbyn Facebook Abomination
#########

fb <- ggplot(corbyn, aes(x = political_group, y = log(avg_facebook_likes))) +
  geom_bar(stat = "identity", width = 1, aes(fill = political_group), color = "black") +
  scale_fill_brewer(palette = "Greens",
                    name = "Never give in, never relevel") +
  theme(plot.title = element_text(size = 30,
                             family = "Times New Roman",
                             color = "green",
                             face = "italic"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8,
                                   color = "white",
                                   angle = 20,
                                   vjust = -0.5),
        axis.title.x = element_text(size = 20,
                                    family = "Courier New",
                                    color = "black"),
        axis.title.y = element_text(size = 14,
                                    color = "navy",
                                    family = "Impact"),
        panel.background = element_rect(color = "black",
                                        fill = "yellow",
                                        size = 2),
        legend.title = element_text(size = 12,
                                    color = "brown4"),
        legend.text = element_text(family = "Times New Roman", size = 11),
        legend.background = element_rect(fill = "tan"),
        panel.grid.major = element_line(color = "red"),
        plot.background = element_rect(fill = "hotpink"),
        plot.margin = margin(t = 50, r = 5, b = 3, l = 20, unit = "pt"),
        strip.background = element_rect(fill = "purple"),
        strip.text = element_text(color = "skyblue",
                                  face = "bold",
                                  family = "Arial Rounded MT Bold",
                                  size = 8)
        ) +
  labs(x = expression(so^(a~superscript^nest)~polar[(a~subscript)]),
       y = "log scale\nsuch easy interpretation",
       title = "Viz Lemon",
       caption = "tag: dataviz comp submission") +
  scale_y_continuous(limits = c(0,10)) +
  coord_polar() +
  facet_wrap(~political_group)


######
# Brexit Gross
######

# Some wrangling:
brexit_2 <- brexit %>%
  mutate(date = dmy(date)) %>% # YAY lubridate!
  gather("response","percent",-date)


# And another rave:
ggplot(brexit_2, aes(x = date, y = percent, group = response)) +
  geom_area(position = "identity",
            aes(fill = response, color = response),
            alpha = 0.5,
            size = 0.7,
            lty = 1) +
  scale_fill_manual(values = c("purple","yellow")) +
  scale_color_manual(values = c("magenta","orange")) +
  coord_cartesian(ylim = c(40, 48)) +
  scale_x_date(expand = c(0,0)) +
  dark_mode(theme_pubclean()) +
  theme(
    legend.position = "top"
  ) +
  labs(x = "Date", y = "Percentage Responding\n(Brexit right or wrong?)")

# Nevermind I want to make on like the better version...

# Wrangling for dates:
brexit_3 <- brexit %>%
  mutate(date = dmy(date))

ggplot(brexit_3) +
  geom_point(aes(x = date, y = percent_responding_right), color = "orange") +
  geom_point(aes(x = date, y = percent_responding_wrong), color = "purple") +
  dark_mode(theme_pubclean()) +
  theme(
    text = element_text(family = "Tahoma")
  ) +
  geom_smooth(aes(x = date,
                  y = percent_responding_right),
              color = "darkorange",
              fill = "orange",
              span = 5) +
  geom_smooth(aes(x = date,
                  y = percent_responding_wrong),
              color = "purple",
              fill = "magenta",
              span = 5) +
  labs(x = "Date",
       y = "Percent responding\n(Brexit reaction)",
       title = "Brexit NO MODEL SELECTION")

# ggsave("viz_lemon.png", width = 6, height = 8)

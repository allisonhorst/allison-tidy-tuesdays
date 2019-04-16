# Tidy Tuesday 4/16/19
# Allison Horst
# Take a pretty nice graph, and make it an absolute abomination


#######
# Load packages
#######

library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(lubridate)
library(ggdark)
library(cowplot)

#######
# Get data (needed here: brexit, corbyn, dogs)
#######

# brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

# corbyn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv")

# dogs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv")

#eu_balance <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")

#pensions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")

#trade <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")

#women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

#########
# Corbyn Facebook Abomination
#########

corbyn_graph <- ggplot(corbyn, aes(x = political_group, y = log(avg_facebook_likes))) +
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
        strip.background = element_rect(fill = "darkgreen"),
        strip.text = element_text(color = "skyblue",
                                  face = "bold",
                                  family = "Arial Rounded MT Bold",
                                  size = 8)
        ) +
  labs(x = "must polar",
       y = "log scale\nsuch easy interpretation",
       title = "So good",
       subtitle = "Artisanal color palette",
       caption = "tag: dataviz comp submission") +
  scale_y_continuous(limits = c(0,10)) +
  coord_polar() +
  facet_wrap(~political_group)


#########
# Brexit MaxGross
#########

# Some wrangling:
brexit_2 <- brexit %>%
  mutate(date = dmy(date)) %>% # YAY lubridate!
  gather("response","percent",-date) # 100th time I've had to learn gather this year...


# And another rave:
brexit_graph_1 <- ggplot(brexit_2, aes(x = date, y = percent, group = response)) +
  geom_area(position = "identity",
            aes(fill = response, color = response),
            alpha = 0.5,
            size = 0.7,
            lty = 1) +
  scale_fill_manual(values = c("purple","yellow"),
                    name = "Brexit:",
                    breaks=c("percent_responding_right", "percent_responding_wrong"),
                    labels=c("It's right!", "It's wrong!")) +
  scale_color_manual(values = c("magenta","orange"),
                     name = "Brexit:",
                     breaks=c("percent_responding_right", "percent_responding_wrong"),
                     labels=c("It's right!", "It's wrong!")) +
  coord_cartesian(ylim = c(40, 48)) +
  scale_x_date(expand = c(0,0),
               breaks = "4 months",
               date_labels = "%b %Y") +
  dark_mode(theme_pubclean()) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Date", y = "Percentage Responding\n(Brexit right or wrong?)")

# Also I want to make one like the better version...

# Wrangling for dates:
brexit_3 <- brexit %>%
  mutate(date = dmy(date))

brexit_graph <- ggplot(brexit_3) +
  geom_point(aes(x = date,
                 y = percent_responding_right),
             color = "orange",
             size = 3,
             pch = 6) +
  geom_point(aes(x = date,
                 y = percent_responding_wrong),
             color = "purple",
             size = 3,
             pch = 5) +
  dark_mode(theme_pubclean()) +
  theme(
    text = element_text(family = "Tahoma"),
    legend.position = "top"
  ) +
  geom_smooth(aes(x = date,
                  y = percent_responding_right),
              color = "darkorange",
              fill = "orange",
              span = 5,
              lty = 6) +
  geom_smooth(aes(x = date,
                  y = percent_responding_wrong),
              color = "purple",
              fill = "magenta",
              span = 5,
              lty = 11) +
  labs(x = "Date",
       y = "Percent of respondents",
       title = "Brexit opinions",
       subtitle = "Who is Loess anyway?") +
  scale_x_date("Date",
               date_labels = "%b %Y",
               date_breaks = "6 months"
               )

brex_graph


###########
# And one more quick one just so I can practice with ggpubr (dogs)
###########

# Gather
dogs_2 <- dogs %>%
  gather("param", "val", -year)

dog_graph <- ggplot(dogs_2, aes(x = year, y = val, group = param)) +
  geom_point(aes(color = param, pch = param), size = 4) +
  geom_line(aes(color = param)) +
  scale_color_manual(values = c("blue","black")) +
  scale_x_continuous(limits = c(2005, 2016),
                     breaks = seq(2005, 2016),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,50),
                     minor_breaks = seq(0,50, by = 2),
                     expand = c(0,0)) +
  theme(legend.position = "top",
        plot.background = element_rect(fill = "lightgoldenrod"),
        axis.text.x = element_text(angle = 90)
        ) +
  labs(x = "Year", y = "Size-o-meter", title = "Adopt a shelter dog!")


##########
# COWPLOT! Multiple graph layouts.
##########

ggdraw() +
  draw_plot(corbyn_graph, x = 0, y = 0.5, height = 0.5, width = 1) +
  draw_plot(dog_graph, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(brexit_graph_1, x = 0.52, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 20, colour = "purple",
                  x = c(0, 0, 0.52), y = c(0.98, 0.5, 0.5))

ggsave("cowplot_test.png", width = 8, height = 10, units = "in")

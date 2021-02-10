############
# Primary Author:Jenna Goldberg
# Creation date: Feb 2, 2021
# Last Modified: Feb 9 2021

# Change Log:
# 02/09 - Changed all labels to white for legibility 

#clear environment 
rm(list = ls())

#load libraries 
library(tidyverse)
library(tigris)
library(here)
library(tidytuesdayR)
library(ggbump)

#load data! 
tuesdata <- tidytuesdayR::tt_load(2021, week = 6)
all_bach <- tuesdata$bach_students

tidy_data <-
  all_bach %>% 
  select(
    Year = Total,
    Pct_Of_All_25plus = `Total, percent of all persons age 25 and over`,
    White = White1,
    Black = Black1,
    Hispanic = Hispanic, 
    Asian = 12,
    `Pacific Islander` = 14,
    `Native American or Alaskan` = 16,
    `Two or more races` = `Two or more race`
  ) %>% 
  pivot_longer(
    cols = 3:9,
    names_to = "Race",
    values_to = "Percentage"
  ) %>% 
  mutate(Percentage = as.numeric(na_if(Percentage, "—"))) %>% 
  drop_na() %>% 
  group_by(Year) %>%
  mutate(rank = rank(-Percentage, ties.method = "first"))

is.even <- function(x) x %% 2 == 0

palette_colors <- 
  c(
    "#C34A36",
    "#4B0000",
    "#FFA084",
    "#9B2619",
    "#6F0000",
    "#EB6C55",
    "#FFD3B4"
  )


plot <- 
  tidy_data %>% 
  filter(is.even(Year)) %>% 
  ggplot(aes(Year, rank, color = Race)) +
  geom_bump(size = 2, smooth = 8) + 
  geom_point(size = 3) +
  geom_text(data = (tidy_data %>%
                      filter(Year == 2016)),
            aes(x = Year + 1, 
                label = paste0(Race, " : ", Percentage, "%")), 
            size = 3, hjust = 0, fontface = "bold",
            color = "white") +
  scale_y_reverse() +
  cowplot::theme_minimal_vgrid() + 
  theme(
    text = element_text(family = "Tahoma"),
    legend.position = "none",
    axis.title.y =  element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 14, color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    plot.caption = element_text(color = "white", hjust = 0.5),
    panel.background = element_rect(fill = "#BEA6A0"),
    plot.background = element_rect(fill = "#BEA6A0")) + 
  scale_x_continuous(
    limits = c(1970, 2030),
    breaks = c(1970, 1980, 1990, 2000, 2010, 2015),
    labels = c("1970", "1980", "1990", "2000", "2010", "2015")
  ) + 
  scale_color_manual(values = palette_colors) +
  labs(
    title = "Ranking the Percentage of Population Under 25 That Had Attained a Bachelors Degree",
    subtitle = "Split by Race, 1970-2016",
    caption = "Data: Data.World  \nGithub: @Jenna-Gold | Twitter: @jennagoldd_ \n  #TidyTuesday\n")

plot

ggsave(
  here("HBCUs", "plot.png"),
  plot,
  width = 10,
  height = 5)

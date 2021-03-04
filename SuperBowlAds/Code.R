############
# Primary Author:Jenna Goldberg
# Creation date: March 1, 2021
# Last Modified: 

# Change Log:

#clear environment 
rm(list = ls())

#load libraries 

library(here)
library(tidytuesdayR)
library(ggiraphExtra)
library(ggradar)
library(tidyverse)

#load data! 
tuesdata <- tidytuesdayR::tt_load(2021, week = 10)

#data cleaning from code by Georgious Karamanis 
clean_data <- 
  tuesdata$youtube %>% 
  add_count(year, name = "year_total") %>% 
  pivot_longer(cols = funny:use_sex,
               names_to = "attribute") %>% 
  filter(value) %>% 
  group_by(year,
           attribute) %>% 
  add_count(value) %>% 
  ungroup() %>% 
  distinct(year, attribute, n, year_total) %>% 
  dplyr::mutate(pct = n / year_total) 

#this seems unecessary - but it helps to fill in the zeroes for when we make it tidy again
wide_data <- 
  clean_data %>% 
  select(year, attribute, pct) %>% 
  pivot_wider(id_cols = year, 
              names_from = attribute,
              values_from = pct,
              values_fill = 0
              )

tidy_data <- 
  wide_data %>% 
  pivot_longer(cols = 2:8,
               names_to = "attribute",
               values_to = "pct") %>% 
  mutate(attribute = 
           case_when(
             attribute == "animals" ~ "Conains Animals",
             attribute == "celebrity" ~ "Contains Celebrity",
             attribute == "danger" ~ "Contains Danger",
             attribute == "funny" ~ "Contains Humor",
             attribute == "patriotic" ~ "Patriotism",
             attribute == "show_product_quickly" ~ "Shows Product Quickly",
             attribute == "use_sex" ~ "Uses Sexuality"
           ))

coord_radar <- 
  function(theta='x', start=0, direction=1){
    # input parameter sanity check
    match.arg(theta, c('x','y'))
    
    ggproto(
      NULL, CoordPolar, 
      theta=theta, r=ifelse(theta=='x','y','x'),
      start=start, direction=sign(direction),
      is_linear=function() TRUE)
  }


facet_plot <- 
  tidy_data %>% 
  arrange(attribute) %>% 
  ggplot(aes(x = as.factor(attribute),
             y = pct,
             group = 1)) +
  geom_polygon(
    color = "#8E0A18",
    fill = "#8E0A18",
    alpha = 0.75,
    lwd = 1.5) +
  facet_wrap(~year, nrow = 3) + 
  ylim(0,1) + 
  coord_radar() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_blank()
  )

facet_plot

ggsave(
  here("SuperBowlAds", "Facet_Plot.png"),
  facet_plot,
  dpi = 1000
)

average_data <- 
  tidy_data %>% 
  group_by(attribute) %>% 
  summarise(pct = mean(pct))

average_plot <- 
  average_data %>% 
  arrange(attribute) %>% 
  ggplot(aes(x = as.factor(attribute),
             y = pct,
             group = 1)) +
  geom_polygon(
    color = "#8E0A18",
    fill = "#8E0A18",
    alpha = 0.75,
    lwd = 1.5) +
  coord_radar() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "black"),
    panel.background = element_blank()
  ) + 
  ylim(0, 1) + 
  labs(title = "Attributes of Super Bowl Commercials",
       subtitle = "Average Percentage of Commercials from 2000-2020")
average_plot
ggsave(
  here("SuperBowlAds", "Average_Plot.png"),
  average_plot,
  dpi = 1000
)

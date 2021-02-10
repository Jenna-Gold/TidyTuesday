############
# Primary Author:Jenna Goldberg
# Creation date: Feb 9, 2021
# Last Modified: 

# Change Log:

#clear environment 
rm(list = ls())

#load libraries 
library(tidyverse)
library(here)
library(tidytuesdayR)
library(glue)
library(ggtext)

#load data! 
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

tidy_data <-
  tuesdata$student_debt %>% 
  inner_join(tuesdata$home_owner)

plot <- 
  tidy_data %>% 
  ggplot() + 
  geom_area(aes(x = year, y = home_owner_pct),
            fill = "#30475e",
            color = "#30475e",
            alpha = 0.8,
            size = 1.0) +
  geom_area(aes(x = year, y = loan_debt_pct), 
            color = "#f05454",
            fill = "#f05454",
            alpha = 0.6, 
            size = 1.0) +
  theme_minimal() + 
  # scale_x_continuous(
  #   limits = c(1989, 2016),
  #   breaks = c(1990, 2000, 2010, 2016),
  #   labels = c("1990", "2000", "2010", "2016")
  # ) + 
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6),
    labels = c("0%", "20%", "40%", "60%")
  ) + 
  cowplot::theme_minimal_hgrid() + 
  facet_wrap(vars(race), ncol = 3, strip.position = "bottom") + 
  labs(title =
         "Contrasting <span style = 'color: #30475e'>Home Ownership</span>
                        and <span style = 'color: #f05454'>Student Debt</span> Rates",
       subtitle = "The shapes of each 'bar' are representative of trends from 1989-2016 for each race.",
       caption = "Data: Urban Institute & U.S. Census  \nGithub: @Jenna-Gold | Twitter: @jennagoldd_",
       y = "Percentage of Households",
       x = "") + 
  theme(
    text = element_text(color = "#222831"),
    plot.title = element_markdown(size = 14, lineheight = 1.2),
    strip.text.x = element_text(size = 12, face = "bold"),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "#dddddd"),
    plot.background = element_rect(fill = "#dddddd")) 
        

ggsave(
  here("Wealth_Inequality", "plot.png"),
  plot
)
#coral : #f05454
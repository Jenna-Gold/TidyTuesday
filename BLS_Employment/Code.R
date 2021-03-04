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
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

# most gendered industries in 2020 
set.seed(7)

clean_data_v1 <- 
  tuesdata$employed %>% 
  filter(race_gender %in% c("Women", "Men") & 
           year == 2020) %>% 
  select(-year) %>% 
  pivot_wider(id_cols = c(1:3, 5),
              names_from = race_gender,
              values_from = employ_n) %>% 
  group_by(industry) %>% 
  summarise(Men = sum(Men, na.rm = T),
            Women = sum(Women, na.rm = T)) %>% 
  mutate(Total = Women + Men,
         Difference = (Women-Men)/Total) %>% 
  filter(Difference != 0) %>% 
  #generate random x value
  mutate(x_value = runif(nrow(.)))

#v1 
clean_data_v1 %>% 
  ggplot() + 
  geom_point(aes(size = Total,
                 x = x_value,
                 y = Difference))

clean_data_v2 <- 
  clean_data_v1 %>% 
  pivot_longer(
    cols = c("Women", "Men"),
    names_to = "Gender",
    values_to = "Number") %>% 
  #set one gender to negative values 
  mutate(
    Number = ifelse(Gender == "Women",
                    Number*-1, 
                    Number),
    industry = fct_reorder(industry, Difference)
  )

label_data <- 
  clean_data_v2 %>% 
  filter(Gender == "Men") %>% 
  select(industry, Number) %>% 
  mutate(Number = 1.5*Number)

ggplot() + 
  geom_bar(
    data = clean_data_v2,
    aes(
      x = industry,
      y = Number,
      fill = Gender
  ),
  stat = "identity"
  ) + 
  coord_flip() + 
  theme_minimal() + 
  scale_x_discrete(position = "top") + 
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length.x = unit(0.8,"cm"),
    plot.background = element_rect(fill = "#DADBDB", color = NA),
    panel.background = element_rect(fill = "#DADBDB", color = NA),
    panel.grid.major.y = element_blank(),
    plot.title = element_markdown(size = 10, lineheight = 1.2, face = "bold", vjust = 40),
    plot.caption = element_text(hjust = 0.5)
  ) + 
  scale_fill_manual(values = c("#4D8076", "#627898")) + 
  labs(
    title =  "Contrasting <span style = 'color: #4D8076'>Male</span>
                        and <span style = 'color: #627898'>Female</span> Employment by Industry",
    caption = "Data: Bureau of Labor Statistics \nGithub: @Jenna-Gold | Twitter: @jennagoldd_ \n  #TidyTuesday\n")

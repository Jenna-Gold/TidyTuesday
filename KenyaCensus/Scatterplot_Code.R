############
# Primary Author:Jenna Goldberg
# Last Editor: 
# Creation date: Jan 19, 2021
# Last Modified: 


# Change Log:

#clear environment 
rm(list = ls())

#load libraries 
library(tidyverse)
library(tigris)
library(here)
library(ggbump)
library(rKenyaCensus)

#load data 
internet_usage <- 
  V4_T2.33 %>% 
  filter(AdminArea == 'County') %>% 
  select(County,
         UoI_Total_Perc,
         Total_Pop = Total)

crops <- V4_T2.20 %>% 
  filter(AdminArea == 'County') %>% 
  mutate(Pct_Households_Farming = 100*(Farming/Total))  %>% 
  select(County,
         Pct_Households_Farming)

#combine data 
full_data <- 
  internet_usage %>% 
  left_join(crops) %>% 
  mutate(flag_nairobi = 
           ifelse(County == "NAIROBI CITY", '1', '0'))
  

#make plot! 
scatter_plot <- 
  full_data %>% 
  ggplot() +
  geom_point(aes(
    size = Total_Pop,
    y = UoI_Total_Perc,
    x = Pct_Households_Farming,
    color = flag_nairobi),
    alpha = 0.5
  ) + 
  scale_colour_manual(values = c("#fff3e6", "#c1a1d3")) +
  theme_classic() + 
  annotate("text", x = 8.5, y = 52.5, label = "Nairobi City",
           color = "#c1a1d3") +
  xlab("Percent of Total Households Practicing Farming") +
  ylab("Percentage of Population that Uses the Internet") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill =  "#0d335d"),
        plot.background = element_rect(fill = "#0d335d"),
        axis.line = element_line(color = "#fff3e6"),
        axis.text = element_text(color = "#fff3e6"),
        axis.title = element_text(color = "#fff3e6"),
        plot.title = element_text(color = "#fff3e6"),
        plot.subtitle = element_text(color = "#fff3e6"),
        plot.caption = element_text(color = "#fff3e6")) + 
  labs(title = "Technology vs. Agriculture",
       subtitle = "Each bubble represents one county in Kenya, with the size being proportional to its population.",
       caption = "Data: Kenya Population & Housing Census 2019 
                  Github: @Jenna-Gold | Twitter: @jennagoldd_ | #TidyTuesday")

#view
scatter_plot

#export
ggsave(scatter_plot, 
       file = here("KenyaCensus", "Final_Plot.png"),
       dpi = 1000)  

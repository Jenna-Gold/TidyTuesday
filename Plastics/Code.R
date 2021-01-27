############
# Primary Author:Jenna Goldberg
# Creation date: Jan 26, 2021
# Last Modified: 

# Change Log:

#clear environment 
rm(list = ls())

#load libraries 
library(tidyverse)
library(tigris)
library(here)
library(tidytuesdayR)

#load data! 
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics


#group countries into continents 
#I did this manually, but probably could've tried to fuzzymatch onto a table
unique(plastics$country)

north_america <- 
  c('Canada',
    'United States of America',
    'Mexico',
    'Honduras',
    'El Salvador')
south_america <- 
  c('Argentina',
    'Brazil',
    'Colombia',
    'ECUADOR',
    'Ecuador',
    'Chile',
    'Peru')
africa <-
  c('Benin',
    'Burkina Faso',
    'Cameroon',
    'Cote D_ivoire',
    'Ghana',
    'Kenya',
    'NIGERIA',
    'Rwanda',
    'South Africa',
    'Tanzania',
    'Tunisia',
    'Kuwait',
    'Nigeria',
    'Togo'
    )
europe <- 
  c('Bulgaria',
    'Cyprus',
    'France',
    'Germany',
    'Ireland',
    'Italy',
    'Latvia',
    'Luxembourg',
    'Montenegro',
    'Netherlands',
    'Portugal',
    'Slovenia',
    'Spain',
    'Switzerland',
    'Turkey',
    'United Kingdom',
    'Denmark',
    'Greece',
    'Lithuania',
    'Romania',
    'Montenegro',
    'Serbia',
    'United Kingdom of Great Britain & Northern Ireland')
asia <-
  c('Bangladesh',
    'Bhutan',
    'China',
    'Hong Kong',
    'India',
    'Japan',
    'Maldives',
    'Sri Lanka',
    "Taiwan_ Republic of China (ROC)",
    'Thailand',
    'Ukraine',
    'United Arab Emirates',
    'Vietnam',
    'Armenia',
    'Korea',
    'Kuwait',
    'Singapore',
    'Indonesia',
    'Malaysia',
    'Philippines'
    )

oceania <-
  c('Australia')

clean_data <- 
  plastics %>% 
  mutate(
    Continent = case_when(
      country %in% oceania ~ "Oceania",
      country %in% europe ~ "Europe",
      country %in% asia ~ "Asia",
      country %in% africa ~ "Africa",
      country %in% north_america ~ "North America",
      country %in% south_america ~ "South America",
      TRUE ~ "ERROR"
    )
  )

# test <- 
#    clean_data %>% 
#    filter(Continent == "ERROR")
# unique(test$country)

#collapse data by continents & pivot_longer to make tidy
tidy_data <- 
  clean_data %>% 
  filter(parent_company != "Grand Total" & 
           year == '2020') %>% 
  group_by(Continent) %>% 
  summarize(across(.cols = c(5:11), sum)) %>% 
  pivot_longer(cols = c(2:8),
               names_to = "Plastic_Type",
               values_to = "Count") %>% 
  group_by(Continent) %>% 
  mutate(Percentage_of_Total = Count/sum(Count),
         #add nicer type lables 
         Type_Desc = case_when(
           Plastic_Type == 'hdpe' ~ 'High density polyethylene',
           Plastic_Type == 'ldpe' ~ 'Low density polyethylene',
           Plastic_Type == 'o' ~ 'Other',
           Plastic_Type == 'pet' ~ 'Polyester',
           Plastic_Type == 'pp' ~ 'Polypropylene',
           Plastic_Type == 'ps' ~ 'Polystyrene',
           Plastic_Type == 'pvc' ~ 'PVC')
         ) 
#to have an empty centre of the graph - 
# I need to add a 'blank row' for each continent 
blank_rows <- 
  data.frame(
   Continent = c('Africa', 'Asia', 'Europe',
      'North America', 'Oceania', 'South America'),
   Plastic_Type = "",
   Count = 0,
   Percentage_of_Total = 0,
   Type_Desc = ""
  )

factor_levels <-
  c('High density polyethylene', 'Low density polyethylene',
    'Polyester', 'Polypropylene',
    'Polystyrene','PVC',
    'Other', '')

final_graph_data <-
  bind_rows(
    tidy_data,
    blank_rows
  ) %>% 
  mutate(Type_Desc = fct_rev(
    fct_relevel(Type_Desc, factor_levels)))

#define palette - sourced from https://mycolor.space/
color_palette <- 
  c(
    "#000000",
    "#34D987",
    "#00C58E",
    "#00B094",
    "#009B96",
    "#008695",
    "#007090",
    "#3272A2"
  )

#make the graph!!!
plot <- 
  final_graph_data %>% 
  ggplot( aes(x = Type_Desc,
              y = Percentage_of_Total) ) +
  geom_bar(aes(fill = Type_Desc), stat="identity") +
  geom_text(hjust = 1, size = 3,
            color = "white",
            aes( y = 0,
                 label = paste(Type_Desc," "))) +
  theme_void() +
  theme(
        text = element_text(family = "Tahoma"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size= 24,
                                  color = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 18,
                                     color = "white"),
        strip.text = element_text(color = "white",
                                  size = 12),
        plot.caption = element_text(color = "white",
                                    hjust = 0.5,
                                    size = 12),
        legend.position = "none",
        panel.background = element_rect(fill =  "black"),
        plot.background = element_rect(fill = "black"),
        ) + 
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Types of Plastic Pollutants Found Around the World",
    subtitle = "Proportions of different types of plastics found during Break Free From Plastic \ncleanups on different continents.\n",
    caption = "Data: Break Free From Plastic \nGithub: @Jenna-Gold | Twitter: @jennagoldd_ \n  #TidyTuesday\n") + 
  coord_polar(theta = "y") +
  ylim(0, 1) +
  facet_wrap(~ Continent) 

ggsave(
  here("Plastics", "plot.png"),
  plot,
  width = 10,
  height = 9)

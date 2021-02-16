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
library(circlize)

#load data! 
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

occupations <- 
  tuesdata$occupation %>% 
  select(Occupation,
         Group, 
         Percentage) %>% 
  group_by(Group) %>% 
  mutate(Occupation = seq(1:5),
         Group = case_when(
           Group == "Whites" ~ "WHITE.",
           Group == "Negroes" ~ "BLACK."
         ))

grid.col <- 
   c(
      "#dc143c",
       "#0000ff",
       "#ffd700",
       "#654321",
       "#d2b48c",
      "Black",
      "Black"
    )

circos.clear()
circos.par(start.degree = 90)
chordDiagram(occupations,
             grid.col = grid.col,
             transparency = 0,
             big.gap = 50,
             small.gap = 10,
             annotationTrack = "grid",
             annotationTrackHeight = c(0.03, 0.01))
circos.info()
circos.track(track.index = 1, panel.fun = function(x, y){
    circos.text(CELL_META$xcenter, CELL_META$ylim[1] + 1, CELL_META$sector.index, 
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          },
  bg.border = NA)  


par(xpd = NA,
    bg = "#ece5d5")

title("\n1900 OCCUPATIONS IN GEORGIA : \nA REIMAGINING OF A GRAPHIC BY W.E.B. DU BOIS.",
      cex = 0.75, font = 4)
legend_text <- 
  c(
    "1. AGRICULTURE, FISHERIES\n AND MINING.",
    "2. MANUFACTURING AND\n MECHANICAL INDUSTRIES.",
    "3. DOMESTIC AND \nPERSONAL SERVICES.",
    "4. PROFESSIONS.",
    "5. TRADE AND\n TRANSPORTATION."
  )
legend_colors <-
  c("#dc143c",
    "#0000ff",
    "#ffd700",
    "#654321",
    "#d2b48c")
legend("bottom", 
       x = 1,
       y = 0.5,
       legend = legend_text,
       fill = legend_colors,
       border = NA,
       bty = "n")



#colors 
#red #dc143c
#gold #ffd700
#blue #0000ff
#green #00aa00
#brown #654321
#tan #d2b48c

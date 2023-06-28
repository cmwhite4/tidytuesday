# Tidy Tuesday - 27 May 2023 - Populated Places
# Author: Christine White

# Load libraries
library(readr)
library(tidyverse)
library(extrafont)
library(ggpubr)
library(ggplot2)
library(ggrepel)
library(maps)

rm(list=ls())

### SET-UP ####

tt <- tidytuesdayR::tt_load('2023-06-27') # Read in data
names <- tt$us_place_names # Get data sets
hist <- tt$us_place_history

#### Identify places with state name in name ######
state_list <- unique(names$state_name) # List of states
temp <- names %>% filter(if_any(2, str_detect, paste0(state_list, collapse = '|'))) %>% # identify features with name in state names list
  filter(endsWith(feature_name, "City")) %>%
  filter(!startsWith(feature_name, "East")) %>%
  filter(!startsWith(feature_name, "North")) # Remove these bc they crowd the map

temp$match <- ifelse(str_detect(temp$feature_name, temp$state_name)==TRUE,"Same State", "Different State") # indicate whether they match or not
temp$stateabb <- state.abb[match(temp$state_name,state.name)] # Get state abbreviation for label
temp$citystate <- paste(temp$feature_name, temp$stateabb, sep = ", ") # Create point labels (city, ST)
  
### PLOT ####
state <- map_data('state') # Load map data

plot <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group = group),
               fill = "honeydew3", color = "papayawhip") + 
  theme(axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank()) + 
  coord_fixed(1.3) + theme_void() + 
  geom_point(data=temp, aes(x=prim_long_dec, y=prim_lat_dec, 
                             shape = match, color = match), size = 3.5) +
  geom_text_repel(data=temp, aes(x=prim_long_dec, y=prim_lat_dec, 
                               label=citystate), size=3) + 
  ggtitle("State City, State",
          subtitle = "Populated places in the US named after the states they are (or aren't) in") +
  scale_color_manual(values = c("salmon3", "grey25")) + 
  theme(panel.background = element_rect(fill = "oldlace", colour = "oldlace",
                                        size = 0.5, linetype = "solid")) + 
  theme(legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.background = element_rect(fill="oldlace",
                                         color = "oldlace"),
        plot.background = element_rect(fill = "oldlace",
                                       color = "oldlace"),
        legend.text = element_text(size=10),
        plot.title = element_text(family = "Didot",
                                  size = 30),
        plot.subtitle = element_text(family = "Didot",
                                     size = 15)) + 
  theme(plot.margin = margin(.5,.5,.5,.5, "cm"))

final_plot <- annotate_figure(plot,
                              bottom = text_grob("Christine White - 28 Jun 2023 - @CM_White2", size = 10, color = "black")) +
  bgcolor("oldlace") # Add footer to plot

setwd('~/Desktop')
ggsave("FinalStatePlot.png", plot = final_plot,
       width = 30, height = 20, units = "cm") # Save final plot

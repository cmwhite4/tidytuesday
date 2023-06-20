# Tidy Tuesday - 20 Jun 2023 - UFO Data
# Author: Christine White

# Load required packages
library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(cowplot)
library(ggpubr)

rm(list=ls())

#### SET-UP ####
# Download TT data
tt <- tidytuesdayR::tt_load('2023-06-20')
sight <- tt$ufo_sightings 
places <- tt$places
day_parts <- tt$day_parts_map

# Plot 1: Sightings Over the Years
plot1 <- ggplot(sight) + 
  geom_histogram(aes(x=as.Date(reported_date_time)),
                                        fill = "snow2", color = "black") +
  scale_y_continuous(labels = c("0", "5k", "10k", "15k", "20k")) +
  scale_x_date(breaks = seq(as.Date("1940-01-01"), as.Date("2050-12-31"), by="40 years"),
               date_labels = "%Y") +
  theme(axis.text = element_text(size = 14, color = "white", face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "white", size = 20, face = "bold", 
                                  hjust = .5, margin = margin(20,0,0,0)),
        plot.subtitle = element_text(color = "white", size = 12, hjust = .5),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black", color = "black")) +
  ggtitle("Year", subtitle = "Total number of sightings")

# Plot 2: Sightings Per Time of Day
plot2_df <- sight %>% drop_na(day_part) %>%
  mutate(day_part2 = str_replace(day_part, ".*dusk.*", "dusk"),
         day_part2 = str_replace(day_part2, ".*dawn.*", "dawn")) %>%
  group_by(day_part2) %>% summarise(n_sight = n(), pct_sight = round(n_sight/93866*100, 0)) %>%
  mutate(temp = "temp") %>% arrange(factor(day_part2, levels = c("dawn","morning", "afternoon", "dusk", "night")))

plot2 <- ggplot(plot2_df, aes(x = temp, y = pct_sight, fill = fct_inorder(day_part2))) +
  geom_col() + 
  scale_y_continuous(breaks = c(4, 55, 78, 91, 98), expand = c(0,0),
                     labels = c("Night", "Dusk", "Afternoon", "Morning", "Dawn")) +
  scale_x_discrete(expand = c(0,0)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "white", size = 14, face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "white", size = 20, face = "bold", 
                                  hjust = .5, margin = margin(20,0,0,0)),
        plot.subtitle = element_text(color = "white", size = 12, hjust = .5),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        legend.position = "none") + 
  scale_fill_manual(values = c("#edf8fb", 
                               "#b3cde3",
                               "#8c96c6", 
                               "#8856a7", 
                               "#810f7c")) + ggtitle("Time of Day", subtitle = "Percentage of all sightings") +
  geom_label(aes(group = fct_inorder(day_part2),
                   label = paste0(pct_sight, "%")),
             color = "black", fill = "white",
            position = position_stack(vjust = 0.5),
            label.size = NA)

# Plot 3: Location of Sightings
world <- map_data("world")

plot3 <- ggplot() +
  geom_map(data = world, map = world,
    aes(long, lat, map_id = region),
    fill = "grey35") + theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.background = element_rect(fill = "black", color = "black"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "black", color = "black"),
            plot.title = element_text(color = "white", size = 20, face = "bold", hjust = .5,
                                      margin = margin(20,0,0,0))) + 
  geom_point(data = places,
    aes(longitude, latitude), color = "slateblue2", alpha = 0.5) +
  ggtitle("Location")

# Combine plots 1 and 2
plot4 <- plot_grid(plot1, plot2, ncol = 1,
          rel_heights = c(.8, 1),
          align = "v")

# Combine combo plot with plot 3
plot5 <- plot_grid(plot4, plot3, rel_widths = c(1, 2))


# Add header and footer
final_plot <- annotate_figure(plot5,
                top = text_grob("UFO Sightings By...", color = "white", face="bold", size = 35),
                              bottom = text_grob("Christine White - 20 Jun 2023 - @CM_White2", size = 12, color = "white")) +
  bgcolor("black")

setwd('~/Desktop')
ggsave("FinalUFOPlot.png", plot = final_plot,
       width = 48, height = 26, units = "cm")


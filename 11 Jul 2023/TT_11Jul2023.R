# Tidy Tuesday - 11 July 2023 - Surface Temperatures
# Author: Christine White

rm(list=ls())

library(tidyverse)
library(extrafont)
library(grid)
library(gridExtra)
library(ggpubr)

### SET-UP ####

tt <- tidytuesdayR::tt_load('2023-07-11') # Read in data
gl <- tt$global_temps # Get data sets
nh <- tt$nh_temps
sh <- tt$sh_temps

# Convert wide to long
gl_long <- gl %>% select(Year:Dec) %>% pivot_longer(Jan:Dec,
                                                    names_to = "Month",
                                                    values_to = "Temp") %>%
  mutate(Hemisphere = "Global",
         YearMonth = paste(Year, Month, sep=""))


nh_long <- nh %>% select(Year:Dec) %>% pivot_longer(Jan:Dec,
                                                     names_to = "Month",
                                                     values_to = "Temp") %>%
  mutate(Hemisphere = "Northern Hemisphere",
         YearMonth = paste(Year, Month, sep=""))

sh_long <- sh %>% select(Year:Dec) %>% pivot_longer(Jan:Dec,
                                                    names_to = "Month",
                                                    values_to = "Temp") %>%
  mutate(Hemisphere = "Southern Hemisphere",
         YearMonth = paste(Year, Month, sep=""))


###### Global plot
gl_plot <- gl_long %>% filter(Year < 2023) %>%
  ggplot() +
  geom_col(aes(x = fct_inorder(factor(YearMonth)), y = Temp, fill = Temp)) +   
  ggtitle("Global") +
  theme_minimal() + scale_x_discrete(breaks = c("1880Jan", "2022Dec"),
                                    labels = c(" ", " ")) + theme(text = element_text(family = "Avenir", color = "grey30"),
                                          axis.text.y = element_blank(),
                                          axis.text.x = element_text(size=16, face = "bold"),
                                          axis.ticks = element_blank(),
                                          panel.border = element_blank(), 
                                          plot.margin = margin(1,1.5,0,1.5, "cm"),
                                          panel.grid = element_blank(),
                                          axis.title = element_blank(),
                                          legend.position = "none",
                                          strip.text.y = element_text(size=16),
                                          plot.title = element_text(size = 16),
                                          plot.subtitle = element_text(size = 12)) +
  scale_fill_gradientn(limits = c(-2, 2), breaks = c(-2, 0, 2),
                       name = "Temperature Deviation", na.value = "grey50",
                       colors = c("#257473", "#64CECC", "#F6DE90", "#E78200", "#A51122"))


##### Northern Hemisphere plot
nh_plot <- nh_long %>% filter(Year < 2023) %>%
  ggplot() +
  geom_col(aes(x = fct_inorder(factor(YearMonth)), y = Temp, fill = Temp)) +
  ggtitle("Northern Hemisphere") + 
  theme_minimal() + scale_x_discrete(breaks = c("1880Jan", "2022Dec"),
                                    labels = c(" ", " ")) + theme(text = element_text(family = "Avenir", color = "grey30"),
                                                                         axis.text.y = element_blank(),
                                                                         axis.text.x = element_text(size=16, face = "bold"),
                                                                         axis.ticks = element_blank(),
                                                                         panel.border = element_blank(),
                                                                         panel.grid = element_blank(),
                                                                         plot.margin = margin(0,1.5,0,1.5, "cm"),
                                                                         axis.title = element_blank(),
                                                                         legend.position = "none",
                                                                         panel.spacing.y = unit(0, "cm"),
                                                                         plot.title = element_text(size = 16),
                                                                         plot.subtitle = element_text(size = 12)) +
  scale_fill_gradientn(limits = c(-2, 2), breaks = c(-2, 0, 2),
                       name = "Temperature Deviation", na.value = "grey50",
                       colors = c("#257473", "#64CECC", "#F6DE90", "#E78200", "#A51122")) +
  guides(fill = guide_colourbar(title.position="bottom"))

##### Southern Hemisphere plot
sh_plot <- sh_long %>% filter(Year < 2023) %>%
  ggplot() +
  geom_col(aes(x = fct_inorder(factor(YearMonth)), y = Temp, fill = Temp)) +
  ggtitle("Southern Hemisphere") + 
  theme_minimal() + scale_x_discrete(breaks = c("1880Jan", "2022Dec"),
                                     labels = c("1880", "2022")) +
  theme(text = element_text(family = "Avenir", color = "grey30"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=16, face = "bold"),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        plot.margin = margin(0,1.5,0,1.5, "cm"),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12)) +
  scale_fill_gradientn(limits = c(-2, 2), breaks = c(-2, 0, 2), na.value = "grey50",
                       colors = c("#257473", "#64CECC", "#F6DE90", "#E78200", "#A51122")) +
  guides(fill = guide_colourbar(title.position="top"))



### Combine plots

gl_nh <- cowplot::plot_grid(gl_plot, nh_plot, ncol=1, rel_heights = c(1,1))

plot <- cowplot::plot_grid(gl_nh, sh_plot, ncol = 1, rel_heights = c(1,.7))


top <- text_grob("Surface Temperatures: 1880 to 2023", color = "grey30", 
                 size = 20, face = "bold", family = "Avenir")

subtitle <- text_grob("Deviation from 1950-1981 Average", color = "grey30", 
                      size = 12, family = "Avenir")

top_plot <- cowplot::plot_grid(top, plot, subtitle, ncol=1, rel_heights = c(.1, 1, .1))

bottom <- text_grob("Christine White - 12 Jul 2023 - @CM_White2", size = 8, color = "grey30")

gg <- cowplot::plot_grid(top_plot, bottom,  ncol=1, rel_heights = c(1, .1))

gg

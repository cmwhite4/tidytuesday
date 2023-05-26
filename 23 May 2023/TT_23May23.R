# Tidy Tuesday - 23 May 2023 - NYC Squirrel Data
# Author: Christine White


# Load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

rm(list=ls())
# Read in data
tt <- read.csv("SquirrelData.csv", na.strings=c("","NA"))

###### Plot A: How many squirrels were seen in Central Park? ####
# Set up x-axis: Day of census
plotA_df <- tt
plotA_df$Day <- as.numeric(substr(plotA_df$Date, start=3, stop = 4)) # Get day from date
plotA_df$Day_c <- plotA_df$Day - (min(plotA_df$Day)-1) # Start count at day 1
plotA_daylabs <- c(rep(c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"), times = 2), "Sat")

# Set up y-axis: Number of differently colored squirrels spotted by day/shift
plotA_sq_perDay <- plotA_df %>% 
  group_by(Day_c, Shift) %>%
  summarise(n_sq = n())

# Set up facets: Shift (morning versus afternoon)
plotA_df$shiftlabs <- recode(tt$Shift, "AM" = "Morning", "PM" = "Afternoon") # Plot labels

# Set up text labels: min/max for each shift
plotA_am_min <- min(plotA_sq_perDay$n_sq[plotA_sq_perDay$Shift=="AM"])
plotA_am_min_x <- plotA_sq_perDay$Day_c[plotA_sq_perDay$n_sq==plotA_am_min]

plotA_am_max <- max(plotA_sq_perDay$n_sq[plotA_sq_perDay$Shift=="AM"])
plotA_am_max_x <- plotA_sq_perDay$Day_c[plotA_sq_perDay$n_sq==plotA_am_max]

plotA_pm_min <- min(plotA_sq_perDay$n_sq[plotA_sq_perDay$Shift=="PM"])
plotA_pm_min_x <- plotA_sq_perDay$Day_c[plotA_sq_perDay$n_sq==plotA_pm_min]

plotA_pm_max <- max(plotA_sq_perDay$n_sq[plotA_sq_perDay$Shift=="PM"])
plotA_pm_max_x <- plotA_sq_perDay$Day_c[plotA_sq_perDay$n_sq==plotA_pm_max]

# Plots!
# 1. Morning
plotA_am_plot <- plotA_df %>% 
  filter(shiftlabs=="Morning") %>% # Subset to just morning
  drop_na(Primary.Fur.Color) %>% # Filter out NAs
  ggplot() + 
  geom_bar(aes(x=Day_c, fill = Primary.Fur.Color), color = "grey25",
           width = .5) + # Make bars thinner
  scale_fill_manual(values = c("Black" = "black",
                               "Gray" = "grey",
                               "Cinnamon"= "tan3")) + theme_bw() + 
  scale_x_continuous(breaks = c(1:15), labels = plotA_daylabs) + 
  xlab("Day of Census") +
  geom_text(aes(label = plotA_am_min, y = plotA_am_min+20, x = plotA_am_min_x),
            color = "grey25") +
  geom_text(aes(label = plotA_am_max, y = plotA_am_max+20, x = plotA_am_max_x),
            color = "grey25") +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = 'lavenderblush', color = "grey25"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 100)) + 
  annotate("text",  x=15, y = 300, label = "Morning", vjust=1, hjust=1,
           size = 10, color = "grey25")

# 2. Afternoon
plotA_pm_plot <- plotA_df %>% filter(shiftlabs=="Afternoon") %>% 
  drop_na(Primary.Fur.Color) %>% ggplot() + 
  geom_bar(aes(x=Day_c, fill = Primary.Fur.Color), color = "black",
           width = 0.5) + 
  scale_fill_manual(values = c("Black" = "black",
                               "Gray" = "grey",
                               "Cinnamon"= "tan3")) + theme_bw() + 
  scale_x_continuous(breaks = c(1:15), labels = plotA_daylabs) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 100)) + 
  xlab("Day of Census") +
  geom_text(aes(label = plotA_pm_min, y = plotA_pm_min+20, x = plotA_pm_min_x),
            color = "white") +
  geom_text(aes(label = plotA_pm_max, y = plotA_pm_max+20, x = plotA_pm_max_x),
            color = "white") +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = 'thistle4', color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=12)) + 
  annotate("text",  x=15, y = 300, label = "Afternoon", vjust=1, hjust=1,
           size = 10, color = "white")

# Combine AM + PM
plotA_ampm <- cowplot::plot_grid(plotA_am_plot, plotA_pm_plot, ncol = 1)

# 3. Legend
plotA_legend_plot <- plotA_df %>% drop_na(Primary.Fur.Color) %>% ggplot() + 
  geom_histogram(aes(x=Day_c, fill = Primary.Fur.Color), color = "black") + 
  scale_fill_manual(values = c("Black" = "black",
                               "Gray" = "grey",
                               "Cinnamon"= "tan3")) + ylab("Number of Squirrels") + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12))

plotA_legend <- cowplot::get_legend(plotA_legend_plot)

plotA_title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "How many squirrels were seen in Central Park?",
    x = 0, hjust = 0, size = 20, fontface = "bold", color = "grey25") +
  theme(plot.margin = margin(0, 0, 0, 7),)

plotA_ampm_title <- cowplot::plot_grid(
  plotA_title, plotA_ampm, ncol = 1, rel_heights = c(0.1, 1))

# Axis title
plotA_ygrob <- textGrob("Number of Squirrels", 
                   gp=gpar(col="black", fontsize=15), rot=90)


plotA_xgrob  <- textGrob("Day of Census", 
                   gp=gpar(col="black", fontsize=15))

# Plot with legend + title + axis titles
plotA_ampm_title_axis <- grid.arrange(arrangeGrob(plotA_ampm_title, left = plotA_ygrob, bottom= plotA_xgrob))

# Final combined plot with legend + title + axis titles
plotA_plot <- grid.arrange(plotA_ampm_title_axis, right = plotA_legend)


###### Plot B: What were they doing? ####
plotB_df <- tt %>% select(Unique.Squirrel.ID, Shift, Primary.Fur.Color, Running:Other.Activities) %>%
  pivot_longer(cols = Running:Other.Activities) %>%
  drop_na() %>%
  mutate(yes_no = ifelse(value=="false", 0, 1)) %>%
  filter(!grepl("dead",value) & !grepl("eat", value) &
           !grepl("chas", value) & !grepl("climb", value)) %>% group_by(name) %>%
  filter(yes_no==1) %>%
  summarise(n=n()) %>%
  mutate(tlabs = recode(name, "Other.Activities"="Other"))

library(treemapify)
plotB_plot <- plotB_df %>%
  ggplot(aes(area = n, fill = n)) + geom_treemap() + 
  geom_treemap_text(aes(label = tlabs), color = "grey25",
                    place = "centre") + 
  scale_fill_gradient(low = "thistle", high = "antiquewhite1") + 
  theme(panel.border = element_rect(colour = "grey25", fill=NA, size=1),
        legend.position="none") + ggtitle("What were they doing?") + 
  theme(plot.margin = margin(2,2,2,2, "cm"),
        plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face="bold",
                                  color = "grey25"))


# Plot C: How were they described?
plotC_df <- tt %>% select(Other.Interactions) %>%
  drop_na() %>%
  mutate(curious = ifelse(grepl("curious", Other.Interactions, ignore.case = TRUE),1, 0),
         shy = ifelse(grepl("shy", Other.Interactions, ignore.case = TRUE),1, 0),
         mad = ifelse(grepl("mad", Other.Interactions, ignore.case = TRUE),1, 0),
         wary = ifelse(grepl("wary", Other.Interactions, ignore.case = TRUE),1, 0),
         indifferent = ifelse(grepl("indifferent", Other.Interactions, ignore.case = TRUE),1, 0),
         observant = ifelse(grepl("observant", Other.Interactions, ignore.case = TRUE),1, 0),
         scared = ifelse(grepl("scared", Other.Interactions, ignore.case = TRUE),1, 0),
         clueless = ifelse(grepl("clueless", Other.Interactions, ignore.case = TRUE),1, 0),
         calm = ifelse(grepl("calm", Other.Interactions, ignore.case = TRUE),1, 0),
         asleep = ifelse(grepl("asleep", Other.Interactions, ignore.case = TRUE),1, 0),
         alert = ifelse(grepl("alert", Other.Interactions, ignore.case = TRUE),1, 0),
         scolding = ifelse(grepl("scolding", Other.Interactions, ignore.case = TRUE),1, 0)) %>%
  select(curious:scolding)

plotC_df2 <- as.data.frame(lapply(plotC_df, sum)) %>% 
  pivot_longer(curious:scolding)

library(ggwordcloud)
plotC_plot <- ggplot(plotC_df2, aes(label = name, size = value)) + 
  geom_text_wordcloud() + theme_minimal() + scale_size_area(max_size = 15) + 
  ggtitle("How were they described?") + theme(plot.margin = margin(2,0,0,0, "cm"),
                                              plot.title = element_text(size = 20,
                                                                        hjust = 0.5,
                                                                        face="bold",
                                                                        color = "grey25"))

library(ggpubr)
plotABC <- ggarrange(plotA_plot,
          ggarrange(plotB_plot, plotC_plot, ncol = 2), # Second row with box and dot plots
          nrow = 2                                      # Labels of the scatter plot
) + bgcolor("white") + theme(plot.margin = margin(2,1,1,1, "cm"))


final_plot <- annotate_figure(plotABC, top = text_grob("Central Park Squirrel Census, 2018", 
                                      color = "orchid4", face="bold", size = 50),
                              bottom = text_grob("Christine White - 26 May 2023 - @CM_White2", size = 12, color = "black")) +
  bgcolor("white")


ggsave("FinalSquirrelPlot.png", plot = final_plot,
       width = 40, height = 35, units = "cm")
     
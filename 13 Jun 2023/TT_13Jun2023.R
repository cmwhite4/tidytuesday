# Tidy Tuesday - 23 May 2023 - NYC Squirrel Data
# Author: Christine White


# Load libraries
library(readr)
library(tidyverse)
library(grid)
library(gridExtra)

rm(list=ls())
# Read in data
tt <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv")

### SET UP ####
# Get # items claimed by household for each village
god <- tt %>% filter(village=="God")
god_items <- as.data.frame(table(unlist(strsplit(god$items_owned, ";"))))
colnames(god_items) <- c("item", "God")

chi <- tt %>% filter(village=="Chirodzo")
chi_items <- as.data.frame(table(unlist(strsplit(chi$items_owned, ";"))))
colnames(chi_items) <- c("item", "Chirodzo")

rua <- tt %>% filter(village=="Ruaca")
rua_items <- as.data.frame(table(unlist(strsplit(rua$items_owned, ";"))))
colnames(rua_items) <- c("item", "Ruaca")

# Merge data sets
new <- full_join(god_items, chi_items, by = "item")
new2 <- full_join(new, rua_items, by = "item") %>%
  filter(item != "NULL") %>%
  pivot_longer(cols = God:Ruaca)

plot_df <- new2 %>% filter(item %in% c("bicycle",
                                       "motorcyle",
                                       "cow_cart",
                                       "car")) %>%
  mutate(label = recode(item,
                        "bicycle" = "Bicycle",
                        "motorcyle" = "Motorcycle",
                        "cow_cart" = "Cow Cart",
                        "car" = "Car"),
           image = recode(item,
                        "bicycle" = "bike.png",
                        "motorcyle" = "motorcycle.png",
                        "cow_cart" = "cow.png",
                        "car" = "car.png"))

# Make NAs into 0s
plot_df[is.na(plot_df)] <- 0

### PLOT ####
# Main plot panel
main <- ggplot(arrange(plot_df, value), aes(x=fct_inorder(label), y = value)) + 
  geom_col(color = "black", fill = "lavender") + theme_classic() + 
  geom_image(aes(image = image), size = .1) + facet_wrap(~name) +
  ylab("Number of Households") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = .5),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 9, face = "bold"),
        strip.text = element_text(size = rel(1.2), face = "bold"),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Number of surveyed households in each village owning each mode of transport",
          subtitle = " ")


# Title panel
top = text_grob("Studying African Farmer-Led Irrigation (SAFI) Survey - 2016-2017", 
                                              color = "black", 
                size = 15, face = "bold", just = "left", hjust = 0.5)

# Author panel
bottom = text_grob("Christine White - 15 Jun 2023 - @CM_White2", size = 8, color = "grey25")

# Combine panels
gg <- gridExtra::arrangeGrob(main, top = top, bottom = bottom)

# Save plot as PNG
ggsave("FinalPlot.png", plot = gg,
       width = 30, height = 14, units = "cm")

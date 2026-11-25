### Libraries
library(tidyverse) # Wrangling
library(ggtext) # Labels
library(ggpubr) # Background image
library(gganimate) # Animate plot
library(gifski) # Make a GIF

### Import data
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-24/locations.csv')
production_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-24/production_locations.csv')
productions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-24/productions.csv')

####### SET UP --------
### Find & Merge ABQ data
abq_loc <- locations |> filter(name == "Albuquerque")
abq_link <- production_locations |> filter(location_id %in% abq_loc$location_id)
abq_prod <- productions |> filter(production_id %in% abq_link$production_id)

### Import gradient image for background
### Resource consulted: https://rpkgs.datanovia.com/ggpubr/reference/background_image.html
img <- png::readPNG("GradientBG.png")

### Specify the sizes and positions for the balloons
plot_dat <- abq_prod |>
  ### X axis is title of TV show in alphabetical order
  arrange(title) |> filter(kind == "tv") |>
  mutate(end_year = case_when(is.na(end_year) ~ year,
                              .default = end_year),
         tri_size = sitelinks / 4,
         squ_size = ifelse(sitelinks < 20, 3, 9),
         text_label = paste0(title, "\n(", year, "-", end_year, ")")) |>
  select(title, year, end_year, sitelinks, tri_size:text_label)

#### Put all the data on a common scale from 2008-2025 so balloons appear in the same frames
plot_dat2 <- plot_dat |>
  slice(rep(row_number(), 18)) |> # repeat each row 18 times
  arrange(title) |>
  mutate(plot_year = rep(seq(2008, 2025, by = 1), 5)) |>
  select(plot_year, title, everything())

#### Develop point positions based on start and end year
##### These are done by hand becauss depending on point size, the need to be more or less offset
plot_dat3 <- plot_dat2 |>
  mutate(bal_point = case_when((plot_year >= year &
                                  plot_year <= end_year) ~ plot_year,
                               (plot_year >= year &
                                  plot_year > end_year) ~ end_year,
                               .default = NA)) |>
  mutate(tri_point = case_when(sitelinks < 20 ~ bal_point - .5,
                               (sitelinks > 20 & sitelinks < 60) ~ bal_point - 1,
                               .default = bal_point - 2),
         squ_point = case_when(sitelinks < 20 ~ bal_point - 1.5,
                               (sitelinks > 20 & sitelinks < 60) ~ bal_point - 3,
                               .default = bal_point - 4),
         text_point = bal_point - 10)

####### PLOT --------
static <- plot_dat3 |>
  ggplot() + 
  ### Add background gradient image
  background_image(img) +
  ### Text Labels
  geom_text(aes(y = text_point, x = title, label = text_label),
            family = "mono", size = 3) +
  ### Point of the balloon (triangle)
  geom_point(aes(x = fct_inorder(title), y = tri_point, color = title, size = tri_size), shape = 25) +
  ### Bulb of the balloon (circle)
  geom_point(aes(x = fct_inorder(title), y = bal_point, color = title, size = sitelinks)) +
  ### Basket  (square)
  geom_point(aes(x = fct_inorder(title), y = squ_point, size = squ_size), shape = 15, color = "#826b41") +
  scale_size_area(max_size = 12) +
  scale_color_manual(values = c("#F5276C", "#F54927",
                                "#F5B027", "#618E57", 
                                "#276CF5")) +
  scale_y_continuous(limits = c(1980, 2040),
                     breaks = c(seq(1980, 2040, by = 10))) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "mono")) +
  ggtitle("TV Series with Filming Locations in Albuquerque, NM")

### Run to save GIF to wd
anim <- static + transition_states(plot_year,
                                    transition_length = 3,
                                    state_length = 1)
animate(anim,
        duration = 4,
        renderer = gifski_renderer("BalloonGIF.gif"))

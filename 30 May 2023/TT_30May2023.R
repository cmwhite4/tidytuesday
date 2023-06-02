# Tidy Tuesday - 30 May 2023 - Centenarian Data
# Author: Christine White

# Load required packages
library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(highcharter)
library(maps)

rm(list=ls())

#### SET-UP ####
# Download and clean TT data
tt <- tidytuesdayR::tt_load('2023-05-30')
tt <- tt$centenarians 

# Cleaning: Change colname to match map data
tt$ISOname <- tt$place_of_death_or_residence

# Cleaning - to match map data for left_join
tt$ISOname[tt$ISOname=="France (French Guiana)"] <- "French Guiana"
tt$ISOname[tt$ISOname=="France (Martinique)"] <- "Martinique"
tt$ISOname[tt$ISOname=="France (Saint Barthélemy)"] <- "Saint Barthelemy"
tt$ISOname[tt$ISOname=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
tt$ISOname[tt$ISOname=="Venezuela"] <- "Venezuela, Bolivarian Republic of"

# Cleaning - to change "NA" for death date to "present"
tt$death_date <- as.character(tt$death_date)
tt$death_date[is.na(tt$death_date)] <- "present"

# Select the oldest person per country 
cent_counts <- tt %>% group_by(ISOname, gender) %>%
  summarise(n_F = sum(gender=="female"),
            n_M = sum(gender=="male")) %>%
  group_by(ISOname) %>% 
  summarise(n_F = sum(n_F), n_M = sum(n_M),
            n_cent = n_F+n_M)

oldest_cent <- tt %>% group_by(ISOname) %>% 
  filter(age==max(age)) %>% select(name, ISOname, birth_date, death_date, age)

new <- left_join(cent_counts, oldest_cent, by = "ISOname")

# Download map data
dat <- iso3166
dat <- rename(dat, "iso-a3" = a3)

# Merge centenarian data with map data
plot_data <- full_join(new, dat, by = "ISOname", relationship = "many-to-many")

colnames(plot_data) <- c("ISOname", "n_F", "n_M", "n_cent", "oldestPerson", "op_BD", "op_DD",
                         "op_age", "a2", "iso-a3", "mapname", "sovereignty")

plot_data$op_age <- floor(plot_data$op_age)


#### PLOT ####

hcmap(
  map = list("custom/world-highres3"),# high resolution world map
  data = plot_data, # name of dataset
  joinBy = "iso-a3",
  showInLegend = TRUE,
  color = "#f1eef8",
  nullColor = "white",
  value = "n_cent",
  name = "Total # of People On Top 100 Lists",
  download_map_data = TRUE
) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(
    text = "Where did the top 100 verified oldest men and women live?",
    style = list(fontWeight = "bold", fontSize = "20px")
  ) %>%
  hc_subtitle(
    text = "Hover over a shaded country for detailed information!"
  ) %>%
  hc_chart(backgroundColor = "#f1eef8")  %>% 
  hc_colorAxis(minColor = "#d5edeb", maxColor = "#1B5652") %>% 
  hc_tooltip(formatter = JS("function(){
                            return ('<b>Country:</b> ' + this.point.ISOname +
                            ' <br> <b> Total Number of People on Top 100 Lists: </b>' + this.point.value +
                            ' <br> <b>Number of Top 100 Oldest Men:</b> ' + this.point.n_M +
                            ' <br> <b>Number of Top 100 Oldest Women:</b> ' + this.point.n_F +
                            ' <br> <b>Oldest Recorded Person</b>: ' + this.point.oldestPerson +
                            ' (' + this.point.op_age + ' years old)')
                            }"))

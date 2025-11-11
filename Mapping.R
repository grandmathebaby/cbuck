#####################################################
#          Thesis Mapping                           #
#          Sacha Medjo-Akono                        #
#          11/3/2025                                #
#####################################################

# clear working space
rm(list=ls())

#load packages
library(tidyverse)
library(ggplot2)
library(scatterpie)
library(maps)
library(mapdata)
library(stringr)
library(viridis)
#### Data ####

# Get California county map data
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

# Read data
properties <- read.csv("Samples.csv")

# Check column names
head(properties)

# Base California map
ca_base <- ggplot(data = ca_county, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray90", color = "white") +
  coord_quickmap() +
  theme_void()

# Add sample locations
ca_map <- ca_base +
  geom_point(
    data = properties,
    mapping = aes(x = long, y = lat),
    color = "red", size = 3, alpha = 0.7,
    inherit.aes = FALSE   # <-- stops inheriting 'group'
  ) +
  labs(title = "Sample Locations in California")

ca_map
#Zoom Ventura LA
ca_map + coord_quickmap(xlim = c(-120, -118), ylim = c(33.5, 35.5))


#Trying again
# --- Libraries ---
library(tidyverse)
library(ggplot2)
library(maps)
library(mapdata)

# --- Map data ---
counties   <- map_data("county")
ca_county  <- subset(counties, region == "california")
states     <- map_data("state")
ca_state   <- subset(states, region == "california")

# --- Custom coordinates ---
ventura_coords <- data.frame(
  county = "Ventura",
  lat = c(34.285084590272994, 34.29082868495036, 34.28310637522082, 
          34.35363647268603, 34.28618305898423),
  long = c(-119.22737820378914, -119.19699414207211, -119.3081245477718, 
           -119.30314909548112, -119.25843803774515)
)

la_coords <- data.frame(
  county = "Los Angeles",
  lat = c(33.96699070199341, 34.112246579395425, 34.11306965860684, 
          34.122239244988215, 34.102135058593795),
  long = c(-118.43640900103553, -118.51037202072598, -118.41143730056167, 
           -118.38467838670132, -118.22260548578195)
)

# Combine both
all_points <- rbind(ventura_coords, la_coords)

# --- Colors ---
ocean_col        <- "#E7ECF0"
land_col         <- "#F4F5F7"
county_line_col  <- "#4B4B4B"   # dark gray county lines
state_border_col <- "#1F1F1F"   # nearly black
ventura_col      <- "#454d21"
la_col           <- "#d68c45"

# --- Map plot ---
epa_map <- ggplot() +
  # background
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = ocean_col, color = ocean_col) +
  
  # counties with dark outlines
  geom_polygon(
    data = ca_county,
    aes(x = long, y = lat, group = group),
    fill = land_col, color = county_line_col, linewidth = 0.6
  ) +
  
  # state border
  geom_polygon(
    data = ca_state,
    aes(x = long, y = lat, group = group),
    fill = NA, color = state_border_col, linewidth = 0.8
  ) +
  
  # points
  geom_point(
    data = all_points,
    aes(x = long, y = lat, fill = county),
    shape = 21, size = 3.5, color = "black", stroke = 0.4, alpha = 0.9
  ) +
  scale_fill_manual(values = c("Ventura" = ventura_col, "Los Angeles" = la_col)) +
  
  # county labels
  geom_text(
    data = data.frame(
      county = c("Ventura", "Los Angeles"),
      long = c(-119.2, -118.35),
      lat = c(34.35, 34.1)
    ),
    aes(x = long, y = lat, label = county),
    color = "#2B2B2B", size = 4.5, fontface = "bold"
  ) +
  
  # map view and theme
  coord_quickmap(xlim = c(-120.5, -118), ylim = c(33.7, 34.7)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = ocean_col, color = ocean_col),
    plot.background  = element_rect(fill = ocean_col, color = ocean_col),
    legend.position = "none",
    plot.title = element_text(color = "#2B2B2B", face = "bold", hjust = 0.02)
  ) +
  labs(title = "Sample Locations in Ventura and Los Angeles Counties")

epa_map

##
library(ggplot2); library(dplyr); library(maps); library(mapdata)

xlim <- c(-120.6, -117.9)
ylim <- c(33.6, 34.8)

# Higher-res coastline from mapdata (fallback)
shore <- map_data("worldHires", xlim = xlim, ylim = ylim)

counties <- map_data("county")
la_ve    <- counties %>%
  filter(region == "california", subregion %in% c("los angeles","ventura"))

labels_df <- la_ve %>%
  group_by(subregion) %>%
  summarise(
    long = mean(range(long)), lat = mean(range(lat)),
    .groups = "drop"
  ) %>% mutate(name = ifelse(subregion == "los angeles", "Los Angeles", "Ventura"))

ocean_col <- "#E7ECF0"; land_col <- "#F4F5F7"
county_line_col <- "#2F2F2F"

ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = ocean_col, color = ocean_col) +
  geom_polygon(data = shore, aes(long, lat, group = group),
               fill = land_col, color = NA) +
  geom_polygon(data = la_ve, aes(long, lat, group = group),
               fill = NA, color = county_line_col, linewidth = 0.7) +
  geom_text(data = labels_df, aes(long, lat, label = name),
            color = "white", size = 5.5, fontface = "bold") +
  geom_text(data = labels_df, aes(long, lat, label = name),
            color = "#222222", size = 4.8, fontface = "bold") +
  coord_quickmap(xlim = xlim, ylim = ylim) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = ocean_col, color = ocean_col),
    plot.background  = element_rect(fill = ocean_col, color = ocean_col)
  ) +
  labs(title = "Ventura & Los Angeles Counties (worldHires shoreline)")

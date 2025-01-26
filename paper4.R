# Paper 4: Road Network in Vietnam 

library(sf) 
library(spData) 
library(tidyverse) 
library(ggplot2)

# Load shapefile of countries
countries <- st_read("/Users/larajoycekaser/Downloads/ne_10m_admin_0_countries_vnm/ne_10m_admin_0_countries_vnm.shx")

# Filter for Vietnam
vietnam <- countries %>% filter(SOVEREIGNT == "Vietnam")

# Check coordinate reference system
st_crs(vietnam)
# WGS 84

# Plot only map of Vietnam
ggplot(data = vietnam) +
  geom_sf(color = "black", fill = "white") +  
  theme_void() +
  labs(title = "Map of Vietnam") 


# Load data of road network for 2014
# https://download.geofabrik.de/asia/vietnam.html
vtroads_2014 <- st_read("/Users/larajoycekaser/Downloads/vietnam-140101-free/gis_osm_roads_free_1.shx")
# LINESTRING and WGS 84


# Load data of road network for 2024
# https://download.geofabrik.de/asia/vietnam.html
vtroads_2024 <- st_read("/Users/larajoycekaser/Downloads/vietnam-240101-free/gis_osm_roads_free_1.shx")
# LINESTRING and WGS 84

# Clip roads to Vietnam's boundary
vtroads_c2014 <- st_intersection(vtroads_2014, vietnam)

#sf_use_s2(F)
vtroads_c2024 <- st_intersection(vtroads_2024, vietnam)

# Types of Roads in datasets 
table(vtroads_c2014$fclass)
table(vtroads_c2024$fclass)

# Define categories

# DISCLAIMER: unsure about categories as they are not the same as in the paper, 
# but for example dual carriageway (a category from the paper) is hard to determine with our variables?
# at bottom of the code I have an alternative proposal of defining the categories

vtroads_c2014 <- vtroads_c2014 %>%
  mutate(road_class = case_when(
    fclass %in% c("motorway", "motorway_link") ~ "Freeways",
    fclass %in% c("trunk", "trunk_link") ~ "National Roads",
    fclass %in% c("primary", "primary_link") ~ "Primary Roads",
    fclass %in% c("secondary", "secondary_link") ~ "Secondary Roads",
    fclass %in% c("tertiary", "residential", "living_street") ~ "Local Roads",
    fclass %in% c("service") ~ "Service Roads",
    fclass %in% c("cycleway", "footway", "path", "pedestrian", "steps") ~ "Non-Motorized Paths",
    fclass %in% c("track", "track_grade1", "track_grade2", "track_grade3", "track_grade4", "track_grade5") ~ "Tracks",
    fclass %in% c("unclassified", "unknown") ~ "Other Roads",
    TRUE ~ "Other Roads"
  ))

vtroads_c2024 <- vtroads_c2024 %>%
  mutate(road_class = case_when(
    fclass %in% c("motorway", "motorway_link") ~ "Freeways",
    fclass %in% c("trunk", "trunk_link") ~ "National Roads",
    fclass %in% c("primary", "primary_link") ~ "Primary Roads",
    fclass %in% c("secondary", "secondary_link") ~ "Secondary Roads",
    fclass %in% c("tertiary", "tertiary_link", "living_street", "residential") ~ "Local Roads",
    fclass %in% c("service") ~ "Service Roads",
    fclass %in% c("cycleway", "footway", "path", "pedestrian", "steps") ~ "Non-Motorized Paths",
    fclass %in% c("track", "track_grade1", "track_grade2", "track_grade3", "track_grade4", "track_grade5") ~ "Tracks",
    fclass %in% c("unclassified", "unknown", "bridleway", "busway") ~ "Other Roads",
    TRUE ~ "Other Roads"
  ))

# Define color palette for road categories
road_colors <- c(
  "Freeways" = "blue",
  "National Roads" = "green",
  "Primary Roads" = "red",
  "Secondary Roads" = "orange",
  "Local Roads" = "gray",
  "Service Roads" = "purple",
  "Non-Motorized Paths" = "lightblue",
  "Tracks" = "brown",
  "Other Roads" = "lightgray"
)

# Plot for 2014
plot_2014 <- ggplot() +
  geom_sf(data = vietnam, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = vtroads_c2014, aes(color = road_class), size = 0.4) +
  scale_color_manual(values = road_colors) +
  theme_void() +
  labs(title = "Road Network in Vietnam (2014)", color = "Road Types") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

# Plot for 2024
plot_2024 <- ggplot() +
  geom_sf(data = vietnam, fill = "white", color = "black", size = 0.5) +
  geom_sf(data = vtroads_c2024, aes(color = road_class), size = 0.4) +
  scale_color_manual(values = road_colors) +
  theme_void() +
  labs(title = "Road Network in Vietnam (2024)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"
  )

# Combine plots, one legend for both 
combined_plot <- plot_2014 + plot_2024 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Comparison of Vietnam's Road Network (2014 vs 2024)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold")))

# Show combined plot
combined_plot










# alternative categories: 
vtroads_c2014 <- vtroads_c2014 %>%
  mutate(road_class = case_when(
    fclass %in% c("motorway", "trunk", "primary") ~ "National Roads",
    fclass %in% c("secondary") ~ "Provincial Roads",
    fclass %in% c("tertiary", "residential", "living_street") ~ "Local Roads",
    TRUE ~ "Other Roads"
  ))
vtroads_c2024 <- vtroads_c2024 %>%
  mutate(road_class = case_when(
    fclass %in% c("motorway", "trunk", "primary") ~ "National Roads",
    fclass %in% c("secondary") ~ "Provincial Roads",
    fclass %in% c("tertiary", "residential", "living_street") ~ "Local Roads",
    TRUE ~ "Other Roads"
  ))
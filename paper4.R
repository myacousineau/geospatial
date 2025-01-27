# Paper 4: Road Network in Vietnam 

library(sf) 
library(spData) 
library(tidyverse) 
library(ggplot2)
library(patchwork)

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
#vtroads_2024 <- st_read("/Users/larajoycekaser/Downloads/vietnam-240101-free/gis_osm_roads_free_1.shx")
# LINESTRING and WGS 84
/Users/larajoycekaser/Downloads/vietnam-200101-free/gis_osm_roads_free_1.shx
vtroads_2020 <- st_read("/Users/larajoycekaser/Downloads/vietnam-200101-free/gis_osm_roads_free_1.shx")

# Clip roads to Vietnam's boundary
vtroads_c2014 <- st_intersection(vtroads_2014, vietnam)
st_geometry_type(vtroads_c2014) 
st_geometry_type(vtroads_2014) 

#sf_use_s2(F)
#vtroads_c2024 <- st_intersection(vtroads_2024, vietnam)
vtroads_c2020 <- st_intersection(vtroads_2020, vietnam)

# Types of Roads in datasets 
table(vtroads_c2014$fclass)
table(vtroads_c2020$fclass)

# Define categories
# Define the road categories
road_categories <- list(
  Freeways = c("motorway", "motorway_link"),
  Dual_Carriageways = c("trunk", "trunk_link"),
  Major_Roads = c("primary", "primary_link", "secondary", "secondary_link"),
  Minor_Roads = c("tertiary", "tertiary_link"),
  Other_Roads = c("service", "living_street", "residential")
)

# Define road types to discard
discard_roads <- c("cycleway", "footway", "path", "pedestrian", "steps",
                   "track", "track_grade1", "track_grade2", "track_grade3", 
                   "track_grade4", "track_grade5", "bridleway", "unknown", "unclassified")

# Filter out discarded road types
vtroads_c2014_filtered <- vtroads_c2014 %>%
  filter(!(fclass %in% discard_roads))

vtroads_c2020_filtered <- vtroads_c2020 %>%
  filter(!(fclass %in% discard_roads))

# Categorize the remaining road types
vtroads_c2014_filtered <- vtroads_c2014_filtered %>%
  mutate(category = case_when(
    fclass %in% road_categories$Freeways ~ "Freeways",
    fclass %in% road_categories$Dual_Carriageways ~ "Dual Carriageways",
    fclass %in% road_categories$Major_Roads ~ "Major Roads",
    fclass %in% road_categories$Minor_Roads ~ "Minor Roads",
    fclass %in% road_categories$Other_Roads ~ "Other Roads",
    TRUE ~ "Unknown" # Catch any unexpected road types
  ))

vtroads_c2020_filtered <- vtroads_c2020_filtered %>%
  mutate(category = case_when(
    fclass %in% road_categories$Freeways ~ "Freeways",
    fclass %in% road_categories$Dual_Carriageways ~ "Dual Carriageways",
    fclass %in% road_categories$Major_Roads ~ "Major Roads",
    fclass %in% road_categories$Minor_Roads ~ "Minor Roads",
    fclass %in% road_categories$Other_Roads ~ "Other Roads",
    TRUE ~ "Unknown" # Catch any unexpected road types
  ))

vtroads_c2014_filtered <- st_cast(vtroads_c2014_filtered, "LINESTRING")
vtroads_c2020_filtered <- st_cast(vtroads_c2020_filtered, "LINESTRING")

road_colors <- c(
  "Freeways" = "#1f77b4",           # Blue
  "Dual Carriageways" = "#2ca02c",  # Green
  "Major Roads" = "#d62728",        # Red
  "Minor Roads" = "#ff7f0e",        # Orange
  "Other Roads" = "#c7c7c7"         # Light gray
)


plot_2014 <- ggplot() +
  # Base map of Vietnam
  geom_sf(data = vietnam, fill = "white", color = "black", size = 0.5) +
  
  # Add other categories (excluding Freeways and Dual Carriageways)
  geom_sf(
    data = vtroads_c2014_filtered %>% filter(!category %in% c("Freeways", "Dual Carriageways")),
    aes(color = category),
    size = 0.4,  # Default line size
    alpha = 0.8  # Slight transparency for less emphasis
  ) +
  
  # Add Freeways and Dual Carriageways on top
  geom_sf(
    data = vtroads_c2014_filtered %>% filter(category %in% c("Freeways", "Dual Carriageways")),
    aes(color = category),
    size = 0.6  # Slightly thicker lines for emphasis
  ) +
  
  # Customize colors and ensure legend uses lines
  scale_color_manual(
    values = road_colors,
    name = "Road Types",
    guide = guide_legend(
      override.aes = list(
        linetype = "solid",  # Ensure lines appear as solid
        size = 1.5           # Set legend line thickness
      )
    )
  ) +
  
  # Minimalistic map styling
  theme_void() +
  
  # Title and legend customization
  labs(title = "Road map 2014") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = c(0, 0.4),                    # Place legend on the left
    legend.justification = c(0, 0.4),            # Position legend below the middle-left
    legend.direction = "vertical",               # Stack legend items vertically
    legend.title = element_text(size = 10, face = "bold"),  # Adjust legend title size
    legend.text = element_text(size = 8),        # Adjust legend text size
    legend.key.height = unit(0.3, "cm"),         # Adjust legend key height
    legend.key.width = unit(1, "cm")             # Adjust legend line width
  )

plot_2020 <- ggplot() +
  # Base map of Vietnam
  geom_sf(data = vietnam, fill = "white", color = "black", size = 0.5) +
  
  # Add other categories (excluding Freeways and Dual Carriageways)
  geom_sf(
    data = vtroads_c2020_filtered %>% filter(!category %in% c("Freeways", "Dual Carriageways")),
    aes(color = category),
    size = 0.4,  # Default line size
    alpha = 0.8  # Slight transparency for less emphasis
  ) +
  
  # Add Freeways and Dual Carriageways on top
  geom_sf(
    data = vtroads_c2020_filtered %>% filter(category %in% c("Freeways", "Dual Carriageways")),
    aes(color = category),
    size = 0.6  # Slightly thicker lines for emphasis
  ) +
  
  # Customize colors and ensure legend uses lines
  scale_color_manual(
    values = road_colors,
    name = "Road Types",
    guide = guide_legend(
      override.aes = list(
        linetype = "solid",  # Ensure lines appear as solid
        size = 1.5           # Set legend line thickness
      )
    )
  ) +
  
  # Minimalistic map styling
  theme_void() +
  
  # Title and legend customization
  labs(title = "Road map 2020") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = c(-0.2, 0.4),                    # Place legend on the left
    legend.justification = c(0, 0.4),            # Position legend below the middle-left
    legend.direction = "vertical",               # Stack legend items vertically
    legend.title = element_text(size = 10, face = "bold"),  # Adjust legend title size
    legend.text = element_text(size = 8),        # Adjust legend text size
    legend.key.height = unit(0.3, "cm"),         # Adjust legend key height
    legend.key.width = unit(1, "cm")             # Adjust legend line width
  )

combined_plot <- plot_2014 + plot_2020 +
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = "Road Maps of Vietnam, 2014 and 2020",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  )

# Display the combined plot
combined_plot
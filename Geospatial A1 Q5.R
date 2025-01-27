rm(list = ls()) # cleaning envrionment
library(readxl)
install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata"))

# Specify the path to your Excel file
excel_file <- "/Users/myacousineau/Desktop/Geo/DTB - 2000.xls"

# Read the Excel file
excel_data <- read_excel(excel_file)

# View the structure of the data
str(excel_data)

# View the first few rows of the data
head(excel_data)

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

brazil_states <- read_state(year = 2000)

brazil_states1 <- brazil_states %>%
  mutate(abbrev_state = ifelse(abbrev_state == "RR", "AM", abbrev_state)) %>%
  mutate(abbrev_state = ifelse(abbrev_state == "AP", "PA", abbrev_state)) %>%
  mutate(abbrev_state = ifelse(abbrev_state == "RO", "MT", abbrev_state)) %>%
  mutate(abbrev_state = ifelse(abbrev_state == "MS", "MT", abbrev_state)) %>%
  mutate(abbrev_state = ifelse(abbrev_state == "DF", "GO", abbrev_state)) %>%
  mutate(abbrev_state = ifelse(abbrev_state == "TO", "GO", abbrev_state))

brazil_states_union <- brazil_states1 %>%
  group_by(abbrev_state) %>%
  summarise(geom = st_union(geom)) %>%
  ungroup() %>%
  mutate(geom = st_make_valid(geom),
         geom = st_simplify(geom, dTolerance = 10)) # Simplify to make the polygons rounder) # Ensure the geometries are valid

library(rmapshaper)
brazil_states_union <- ms_simplify(brazil_states_union)

highways <- st_read("/Users/myacousineau/Downloads/Rodovias_MT/SNV_202410A.shp")
highways <- st_transform(highways, crs = 5880)

summary(highways)
library(stringr)
radial_highways <- highways %>%
  filter(str_starts(vl_br, "0"))  # Filter rows where vl_br starts with '0'

filtered_highways <- highways %>%
  filter(ds_legenda == "Pavimentada")

# Coordinates for Brasília and all other cities in the groups
city_coords <- data.frame(
  city = c("Brasilia", "Porto Velho", "Manaus", "Boa Vista",
           "Cuiaba", "Rio Branco", "Goiania", "Campo Grande",
           "Belo Horizonte", "Vitoria", "Rio de Janeiro", 
           "Sao Paulo", "Curitiba", "Florianopolis", "Porto Alegre", 
           "Salvador", "Teresina", "Fortaleza", "Natal", "Joao Pessoa", 
           "Recife", "Maceio", "Aracaju", "Sao Luis", "Belem", "Macapa", "Palmas"),
  latitude = c(-15.7801, -8.7612, -3.1190, 2.8236,
               -15.6010, -9.0283, -16.6869, -20.4697,
               -19.9191, -20.3155, -22.9068, -23.5505, -25.4297, -27.5954, -30.0331,
               -12.9714, -5.0919, -3.7172, -5.7945, -7.1195, -7.2106, -9.666, -10.9472,
               -2.5293, -1.4481, 0.0355, -10.2491), # Adding new cities' latitudes
  longitude = c(-47.9292, -63.9028, -60.2093, -60.6753,
                -56.0969, -67.8888, -49.2641, -54.6201,
                -43.9378, -40.3128, -43.1729, -46.6333, -49.2719, -48.5497, -51.2251,
                -38.5013, -42.7757, -38.5434, -35.2110, -34.8469, -34.88, -35.7213, -37.0733,
                -44.3028, -48.5026, -51.2111, -48.3243) # Adding new cities' longitudes
)

city_coords_sf <- st_as_sf(city_coords, coords = c("longitude", "latitude"), crs = 4326)

# Define a function to compute MST for a group of cities
compute_mst <- function(city_group, city_coords) {
  # Filter the coordinates for the selected city group
  city_group_coords <- city_coords %>% filter(city %in% city_group)
  
  # Calculate the pairwise distance matrix (geodesic distance)
  dist_matrix <- distm(city_group_coords[, c("longitude", "latitude")], fun = distGeo)
  
  # Create a graph from the distance matrix
  city_graph <- graph_from_adjacency_matrix(dist_matrix, mode = "undirected", weighted = TRUE)
  
  # Compute the Minimum Spanning Tree using Prim's algorithm
  mst <- mst(city_graph, algorithm = "prim", weights = E(city_graph)$weight)
  
  # Extract the edges of the MST
  mst_edges <- as_data_frame(mst, what = "edges")
  
  # Extract coordinates for the MST lines
  mst_lines <- lapply(1:nrow(mst_edges), function(i) {
    from_city <- city_group_coords[mst_edges$from[i], c("longitude", "latitude")]
    to_city <- city_group_coords[mst_edges$to[i], c("longitude", "latitude")]
    
    # Ensure these are numeric vectors
    from_coords <- as.numeric(from_city)
    to_coords <- as.numeric(to_city)
    
    # Create a line for each edge
    st_sfc(st_linestring(rbind(from_coords, to_coords)), crs = 4326)
  })
  
  # Convert the list of lines to an sf object
  mst_sf <- st_sf(geometry = do.call(c, mst_lines))
  
  return(list(mst_sf = mst_sf, city_group_coords = city_group_coords))
}

# List of city groups including the new group (Brasilia, Sao Luis, Belem, Macapa)
city_groups <- list(
  c("Brasilia", "Porto Velho", "Manaus", "Boa Vista"),
  c("Brasilia", "Cuiaba", "Rio Branco"),
  c("Brasilia", "Goiania", "Campo Grande"),
  c("Brasilia", "Belo Horizonte", "Vitoria", "Rio de Janeiro"),
  c("Brasilia", "Sao Paulo", "Curitiba", "Florianopolis", "Porto Alegre"),
  c("Brasilia", "Salvador"),
  c("Brasilia", "Teresina", "Fortaleza", "Natal", "Joao Pessoa", "Recife", "Maceio", "Aracaju"),
  c("Brasilia", "Palmas", "Sao Luis", "Belem", "Macapa") # New group added here
)

# Loop over each group, compute MST and store results
mst_results <- lapply(city_groups, function(group) {
  compute_mst(group, city_coords)
})


# Combine all MST lines into a single sf object and add a column for legend
all_mst_lines <- do.call(rbind, lapply(mst_results, function(result) {
  result$mst_sf
}))

# Add a column to define the MST line category
all_mst_lines$line_type <- "MST Line"

# Plot Brazilian states, highways, and cities with labels, including MST lines with a legend
ggplot() +
  # Plot states
  geom_sf(data = brazil_states_union, fill = "white", color = "grey80") +
  # Plot filtered highways with color mapped to "Non-radial highways"
  geom_sf(data = filtered_highways, aes(color = "Non-radial highways", linetype = "Non-radial highways"), size = 0.1) +
  # Plot radial highways with color mapped to "Radial highways"
  geom_sf(data = radial_highways, aes(color = "Radial highways", linetype = "Radial highways"), size = 1) +
  # Plot city points
  geom_sf(data = city_coords_sf, size = 2, shape = 21, fill = "black") +
  # Add city labels, nudging certain cities below
  geom_text(data = city_coords_sf, 
            aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = city), 
            nudge_y = ifelse(city_coords_sf$city %in% c("Sao Paulo", "Recife", "Manaus", "Rio Branco", "Belem", "Sao Luis", "Maceio", "Aracaju", "Salvador", "Goiania", "Campo Grande", "Teresina", "Curitiba", "Florianopolis","Porto Alegre"), -1, 1), 
            size = 3.5, color = "black") +  # Adjust size and color
  # Plot MST lines with color and linetype mapped to "Minimum spanning tree"
  geom_sf(data = all_mst_lines, aes(color = "Minimum spanning tree", linetype = "Minimum spanning tree"), size = 0.1) +
  # Customize the legend for both color and linetype
  scale_color_manual(
    name = "", 
    values = c("Non-radial highways" = "grey70", 
               "Radial highways" = "grey30", 
               "Minimum spanning tree" = "grey50")
  ) +
  scale_linetype_manual(
    name = "", 
    values = c("Non-radial highways" = "solid", 
               "Radial highways" = "solid", 
               "Minimum spanning tree" = "dotted")
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank()   # Remove axis labels
  )  # Adjust position of the legend




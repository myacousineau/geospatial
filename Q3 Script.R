#Assignment 1 Q3

rm(list = ls())

library(sf)
library(tidyverse)
library(geobr) #contains spatial data on brazil 
library(ggplot2)
library(patchwork)

#Read in Data
setwd("C:/Users/micha/OneDrive/Desktop/Master's 2nd Semester/Geospatial/Assignment1/Q3")

population_data <- st_read("pop.xls") %>%
  mutate(Codigo = as.numeric(Codigo))

#Subset for easier use map by map later
pop_1950 <- population_data %>%
  select(-c(X1960, X1970, X1980, X1991, X1996, X2000, X2007, X2010))

pop_1980 <- population_data %>%
  select(-c(X1950, X1960, X1970, X1991, X1996, X2000, X2007, X2010))

pop_2010 <- population_data %>%
  select(-c(X1950, X1960, X1970, X1980, X1991, X1996, X2000, X2007))

#Municpality level geo-spatial data 


#Ealiest date available is 2000. Assume fixed meso-region and use 2010 for all. 

meso_2010 <- read_meso_region( 
  year = 2010,
  showProgress = FALSE) 

meso_2010 <- meso_2010 %>%
  rename(Codigo = code_meso)

meso_1980 <- meso_2010
meso_1950 <- meso_2010

states <- read_state(
  year=2010, 
  showProgress = FALSE)

#Just West:
states <- states %>%
  filter(name_region != "Sul") %>%
  filter(name_region != "Sudeste") %>%
  filter(name_region != "Nordeste") 

#Boundary 
west_boundary <- st_union(states$geom)

#------------------------------------------------------------------------------#
#1950 DATA Cleaning
#------------------------------------------------------------------------------#

data_1950 <- meso_1950 %>%
  merge(pop_1950, by = "Codigo")

#Checking Missing vakues 
missing_values <- sum(is.na(data_1950$geom))
print(missing_values) #no missings

#Checking unique geometries 
all_unique <- length(data_1950$geom) == length(unique(data_1950$geom)) #Not unique 
all_unique
#------------------------------------------------------------------------------#
#1980 DATA Cleaning
#------------------------------------------------------------------------------#
data_1980 <- meso_1980 %>%
  merge(pop_1980, by = "Codigo")

#Checking Missing vakues 
missing_values <- sum(is.na(data_1980$geom))
print(missing_values) #no missings

#Checking unique geometries 
all_unique <- length(data_1980$geom) == length(unique(data_1980$geom)) 
print(all_unique) #unique 

#------------------------------------------------------------------------------#
#2010 DATA Cleaning
#------------------------------------------------------------------------------#
data_2010 <- meso_2010 %>%
 merge(pop_2010, by = "Codigo")

#Checking Missing vakues 
missing_values <- sum(is.na(data_2010$geom))
print(missing_values) #no missings

#Checking unique geometries 
all_unique <- length(data_2010$geom) == length(unique(data_2010$geom)) 
print(all_unique) #unique 

#------------------------------------------------------------------------------#
#Population Shares
#------------------------------------------------------------------------------#


data_1950 <- data_1950 %>% 
  mutate(total_pop = sum(X1950, na.rm=TRUE)) %>%
  mutate(pop_share = (100 * X1950/total_pop))

data_1980 <- data_1980 %>% 
  mutate(total_pop = sum(X1980, na.rm=TRUE)) %>%
  mutate(pop_share = (100 * X1980/total_pop))

data_2010 <- data_2010 %>% 
  mutate(total_pop = sum(X2010, na.rm=TRUE)) %>%
  mutate(pop_share = (100 * X2010/total_pop))

#------------------------------------------------------------------------------#
#Plotting 
#------------------------------------------------------------------------------#
# Plot for 1950
plot_1950 <- 
  ggplot(data_1950) +
  geom_sf(aes(fill = pop_share)) +
  geom_sf(data = west_boundary, fill = NA, color = "red", size = 10000) +  # Correct west_boundary layer
  scale_fill_stepsn(
    name = "Pop. Share",
    colors = c("#e6f2ff", "#66b2ff", "#1a75ff", "#004c99", "#00264d"),
    breaks = c(0.00167, 0.146, 0.349, 0.615, 1.08, 6.59),
    na.value = "white"
  ) +
  labs(title = "(a) 1950") +
  theme_minimal()

# Plot for 1980
plot_1980 <- 
  ggplot(data_1980) +
  geom_sf(aes(fill = pop_share)) +
  geom_sf(data = west_boundary, fill = NA, color = "red", size = 10000) +  # Correct west_boundary layer
  scale_fill_stepsn(
    name = "Pop. Share",
    colors = c("#e6f2ff", "#66b2ff", "#1a75ff", "#004c99", "#00264d"),
    breaks = c(0.0102, 0.185, 0.393, 0.604, 1.02, 11.3),
    na.value = "white"
  ) +
  labs(title = "(b) 1980") +
  theme_minimal()

# Plot for 2010
plot_2010 <- 
  ggplot(data_2010) +
  geom_sf(aes(fill = pop_share)) +
  geom_sf(data = west_boundary, fill = NA, color = "red", size = 10000) +  # Correct west_boundary layer
  scale_fill_stepsn(
    name = "Pop. Share",
    colors = c("#e6f2ff", "#66b2ff", "#1a75ff", "#004c99", "#00264d"),
    breaks = c(0.0272, 0.216, 0.382, 0.557, 1.03, 11.1),
    na.value = "white"
  ) +
  labs(title = "(c) 2010") +
  theme_minimal()

# Combine plots
combined_plot <- plot_1950 + plot_1980 + plot_2010 +
  plot_layout(ncol = 3) & theme(legend.position = "right")

# Display combined plot
print(combined_plot)

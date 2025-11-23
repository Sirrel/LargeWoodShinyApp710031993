######################################################
## Practical 6 for GEOM184 - Open Source GIS ##
## 14/11/2025 ##
## Creating a ShinyApp ##
## Global.R ##
## Code by Ryan Reeve ##
######################################################

# G1 Load large wood, river, bridge, dam, catcher, and risk data ----
lw_points <- st_read("LW_Identification.shp")
river <- st_read("RiverIsonzo.shp")
bridges <- st_read("BridgesIsonzo.shp")
dams <- st_read("Dams.shp")
lw_catchers <- st_read("LW_Catchers.shp")
bridge_risk <- st_read("bridge_risk_index_square.shp")

# Convert vectors to CRS 4326
lw_points <- st_transform(lw_points, crs = 3857)
river <- st_transform(river, crs = 4326)
bridges <- st_transform(bridges, crs = 4326)
dams <- st_transform(dams, crs = 4326)
lw_catchers <- st_transform(lw_catchers, crs = 4326)
bridge_risk <- st_transform(bridge_risk, crs = 4326)

# Perform clustering
lw_coords <- st_coordinates(lw_points)
clusters <- dbscan(lw_coords, eps = 2000, minPts = 3)
lw_points$clusters <- as.factor(clusters$cluster)
# Reproject back to CRS 4326
lw_points <- st_transform(lw_points, crs = 4326)

# Dynamically generate colours based on number of unique clusters
num_clusters <- length(unique(lw_points$clusters))
pal_clusters <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Paired"))(num_clusters),domain = lw_points$clusters)

## G2 Add Raster Layers ----

# Add heatmap raster
heatmap <- rast("heatmap_raster_20px.tif")
heatmap <- project(heatmap, crs(river))
pal_heatmap <- colorNumeric(palette = "inferno", domain=na.omit(values(heatmap)), na.color = "transparent")

# Add slope raster
slope <- rast("slope.tif")
slope <- project(slope, crs(river))
# Create colour palette
pal_slope <- colorNumeric(palette = "mako", domain=na.omit(values(slope)), na.color = "transparent")

# Add aspect raster
aspect <- rast("aspect.tif")
aspect <- project(aspect, crs(river))
# Create colour palette
pal_aspect <- colorNumeric(palette = "viridis", domain=na.omit(values(aspect)), na.color = "transparent")

# Convert rasters for image query
slope <- raster(slope)
aspect <- raster(aspect)

## G3 Implement Bridge Risk Data ----

# Add weight and risk levels for bridge risk index based on LW count (id_count)
bridge_risk <- bridge_risk %>%
  mutate(
    # Create 'weight' column
    weight = case_when(
      id_count <= 4 ~ 1,
      id_count <= 8 ~ 2,
      TRUE ~ 3  
    ),
    risk = case_when(
      id_count <= 4 ~ "Low",
      id_count <= 8 ~ "Medium",
      TRUE ~ "High"  
    )
  ) %>%
  # Convert to factors
  mutate(
    weight = as.factor(weight),
    risk = as.factor(risk)
  )

## G4 Prepare environmental explorer panel data ----

# Extract raster values at large wood locations
lw_points$slope_val <- terra::extract(slope, lw_points)
lw_points$aspect_val <- terra::extract(aspect, lw_points)

# Create a summary table for aspect, binning data into directional categories
aspect_summary <- lw_points %>%
  st_drop_geometry() %>% # Remove geometry as only column is needed
  filter(!is.na(aspect_val)) %>%
  mutate(
    # Define the 8 directions
    direction_bin = cut(aspect_val,
                        breaks = c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
                        labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N (Other)"),
                        right = TRUE, include.lowest=TRUE)
  ) %>%
  # Group by new bins and count
  group_by(direction_bin) %>%
  summarise(count = n()) %>%
  # Combine the two "N" categories
  group_by(direction_bin = ifelse(direction_bin == "N (Other)", "N", as.character(direction_bin))) %>%
  # Add a factor to get the order right
  mutate(direction_bin = factor(direction_bin,
                                levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))

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

## G2 Topographic Analysis ----
# G2.1 Add the DTM ----
DTM <- rast("DTM_Isonzo.tif") 
# Ensure CRS matches the river network
DTM <- project(DTM, crs(river)) # Reproject the raster on the same CRS
lw_points <- st_transform(lw_points, crs(river)) # Reproject the raster on the same CRS
DTM <- ifel(DTM == 0, NA, DTM) # Remove any 0 values

DTM_clip <- crop(DTM, bbox)

# Compute aspect and slope
aspect <- terrain(DTM_clip, "aspect", unit="degrees")
slope <- terrain(DTM_clip, "slope", unit="degrees")
# Convert to dataframe
aspect_df <- as.data.frame(aspect, xy= TRUE)
slope_df <- as.data.frame(slope, xy= TRUE)

# Define function to snap bridges to rivers
snap_to_river <- function(points, river_line) {
  nearest <- st_nearest_points(points, river_line)
  pts <- st_cast(nearest, "POINT")
  # Keep only the snapped points (every second point in the returned sequence)
  pts <- pts[seq(2, length(pts), 2)]
  st_sf(st_drop_geometry(points), geometry= pts, crs = st_crs(points))
}
# Apply to bridges
bridges_snapped <- snap_to_river(bridges, river)
bridges_snapped$id <- seq_len(nrow(bridges_snapped))

lw_points <- st_transform(lw_points, crs(river))

# Define function to extract upstream river segment from bridge
get_upstream_DEMzone <- function(bridge, river, lw_points, slope_rast, aspect_rast,
                                 buffer_length = 2000, buffer_width = 200) {
  # bridge: single-row sf POINT
  # river: river sf (may contain multiple features)
  # lw_points: sf POINT layer of large wood
  # slope_rast, aspect_rast: terra SpatRast layers
  # buffer_length: distance upstream along the river to include (m)
  # buffer_width: lateral buffer around the upstream reach (m)
  
  # 1. Bridge geometry
  b_geom <- st_geometry(bridge)[[1]]
  
  # 2. Choose the nearest river feature to the bridge
  idx <- which.min(st_distance(river, bridge))
  r_geom <- st_geometry(river[idx, ])[[1]] #sfg LINESTRING
  
  # 3. Fractional position of the bridge along the river (0 = start, 1 = end)
  pos_frac <- tryCatch({
    st_line_locate_point(r_geom, b_geom) %>% as.numeric() # lwgeom method
  }, error = function(e) {
    # Fallback: approximate by nearest vertex fraction
    coords <- st_coordinates(r_geom)
    cumdist <- cumsum(c(0, sqrt(diff(coords[,1])^2 + diff(coords[,2])^2)))
    b_coords <- st_coordinates(b_geom)
    nearest_vertex <- which.min(sqrt((coords[,1]-b_coords[1])^2 + (coords[,2]-b_coords[2])^2))
    cumdist[nearest_vertex] / max(cumdist)
  })
  
  # 4. Compute start fraction for upstream segment
  total_len <- as.numeric(st_length(r_geom))
  frac_start <- max(0, pos_frac - buffer_length / total_len)
  
  # 5. Extract upstream segment (try lwgeom's substring, fallback to vertex subset)
  upstream_segment <- tryCatch({
    st_line_substring(r_geom, from = frac_start, to = pos_frac)
  }, error = function(e) {
    coords <- st_coordinates(r_geom)
    cumdist <- cumsum(c(0, sqrt(diff(coords[,1])^2 + diff(coords[,2])^2)))
    keep <- which(cumdist >= frac_start * max(cumdist) &
                    cumdist <= pos_frac * max(cumdist))
    if (length(keep) < 2) keep <- c(max(1, min(keep)-1), min(nrow(coords), max(keep)+1))
    st_sfc(st_linestring(coords[keep, c("X","Y")]), crs = st_crs(river))
  })
  upstream_segment <- st_sf(geometry = st_sfc(upstream_segment, crs = st_crs(river)))
  
  # 6. Buffer to create an analysis zone and count LW inside
  zone <- st_buffer(upstream_segment, dist = buffer_width)
  lw_count <- sum(st_intersects(zone, lw_points, sparse = FALSE))
  
  # 7. Extract slope & aspect values along the upstream segment
  seg_pts <- st_cast(upstream_segment, "POINT")
  if (length(seg_pts) >= 1) {
    coords <- st_coordinates(seg_pts)
    s_vals <- terra::extract(slope_rast, coords[, c("X", "Y")])
    s_vals <- s_vals[, !names(s_vals) %in% "ID", drop = TRUE]
    a_vals <- terra::extract(aspect_rast, coords[, c("X", "Y")])
    a_vals <- a_vals[, !names(a_vals) %in% "ID", drop = TRUE]
    mean_s <- mean(s_vals, na.rm = TRUE)
    mean_a <- mean(a_vals, na.rm = TRUE)
  } else {
    mean_s <- NA_real_; mean_a <- NA_real_
  }
  
  list(zone = zone, lw_count = lw_count, mean_slope = mean_s, mean_aspect = mean_a)
}

# Create empty lists that will need filling later in the loop
bridge_zones <- list()
bridges_snapped$LW_upstream <- NA_integer_
bridges_snapped$mean_slope <- NA_integer_
bridges_snapped$mean_aspect <- NA_integer_

# Run the loop
for (i in seq_len(nrow(bridges_snapped))) {
  res <- get_upstream_DEMzone(bridges_snapped[i, ], river, lw_points, slope,
                              aspect, buffer_length = 2000, buffer_width = 300)
  bridges_snapped$LW_upstream[i] <- res$lw_count
  bridges_snapped$mean_slope[i] <- res$mean_slope
  bridges_snapped$mean_aspect[i] <- res$mean_aspect
  bridge_zones[[i]] <- res$zone
  if (i %% 5 == 0) cat("Processed", i, "of", nrow(bridges_snapped), "bridges\n")
}

upstream_zones <- do.call(rbind, bridge_zones)
upstream_zones <- st_sf(
  bridge_id = bridges_snapped$id,
  lw_count = bridges_snapped$LW_upstream,
  geometry = st_geometry(upstream_zones),
  crs = st_crs(river)
)

## G3 Add Raster Layers ----

# Add heatmap raster
heatmap <- rast("heatmap_raster_10px.tif")
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

## G4 Implement Bridge Risk Data ----

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

## G5 Prepare environmental explorer panel data ----

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

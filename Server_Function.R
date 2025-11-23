######################################################
## Practical 6 for GEOM184 - Open Source GIS ##
## 14/11/2025 ##
## Creating a ShinyApp ##
## Server.R ##
## Code by Ryan Reeve ##
######################################################

## S0 Get Risk Colours and Icons ----
getColor <- function(bridge_risk) {
  sapply(bridge_risk$id_count, function(id_count) {
    if (id_count <= 4) {
      "darkgreen"
    } else if (id_count >= 5 & id_count <= 8) {
      "orange"
    } else {
      "darkred"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(bridge_risk)
)

## S1 Welcome Dialog
showModal(modalDialog(
  title = h2("Welcome to the Isonzo River Large Wood Risk App", align = "center"),
  
  # App description
  p("This application is intended as a visualisation tool to help understand the distribution
    of large wood in the River Isonzo, with supplementary analysis to aid in decision-making
    for the implementation of large wood accumulation mitigation strategies. Structural bridge
    risk is an important concern and understanding where optimal locations are for mitigation
    is crucial for limiting the damage to bridges."),
  br(),
  p("Use this interactive application and analytical tabs to explore environmental correlations, risk hotspots, and proposed mitigation strategies."),
  hr(),
  
  # Instructional content
  h4("How to Use the Interactive Map", style = "color: #337ab7;"),
  p("This application visualises instream large wood on the River Isonzo"),
  p("Please allow a moment or two for data to load and expand the window size for the best experience."),
  tags$ul(
    tags$li(strong("Map Interaction:"), " Pan and zoom to see the plot update dynamically based on your current view."),
    tags$li(strong("Layer Control:"), " Use the layers control box to toggle between different data."),
    tags$li(strong("Data Exploration:"), " Click on the large wood and bridge data for popup details. Hover to view slope and aspect data displayed in the top-right."),
    tags$li(strong("Interactive Plot:"), " The plot in the bottom-right updates with your current map view."),
    tags$li(strong("Measurements:"), " Use the measure tool in the top-left to measure distances and areas."),
    tags$li(strong("Instructions:"), " This information is also availabe in the Instructions tab.")
  ),
  # Proceed button
  footer = modalButton("Proceed to Map"),
  
  size = "l",
  easyClose = TRUE
))

## S2 Render leaflet map ----
# Leaflet map output
output$map <- renderLeaflet({
  leaflet() %>%
    setView(lng=13.533545, lat=45.850065, zoom=11.3) %>%
    addProviderTiles(providers$OpenStreetMap, group = "Colour") %>%
    addPolylines(data = river, color = "blue", weight = 2, opacity = 0.8, group = "River") %>%
    addCircleMarkers(data = dams, color = "hotpink", weight = 2, opacity = 0.8, group = "Dams", label = "Dam") %>%
    addCircleMarkers(data = lw_catchers, color = "purple", weight = 3, opacity = 0.6, group = "Large Wood Catchers", label = "Catcher") %>%
    addAwesomeMarkers(data = bridge_risk, icon = icons, group = "Bridge Risk Index", label = ~as.character(bridge_risk$risk)) %>%
    addRasterImage(slope, colors = pal_slope, opacity = 0.7, group = "Slope", maxBytes = 1.5e+7) %>%
    addRasterImage(aspect, colors = pal_aspect, opacity = 0.7, group = "Aspect", maxBytes = 1.5e+7) %>%
    addRasterImage(heatmap, colors = pal_heatmap, opacity = 0.7, group = "Heatmap") %>%
    addImageQuery(
      slope,
      layerId = "Slope",
      prefix = "Value: ",
      digits = 2,
      position = "topright",
      type = "mousemove", # Show values on mouse movement
      options = queryOptions(position="topright"),
      group = "Slope"
    ) %>%
    addImageQuery(
      aspect,
      layerId = "Aspect",
      prefix = "Value: ",
      digits = 2,
      position = "topright",
      type = "mousemove", # Show values on mouse movement
      options = queryOptions(position="topright"),
      group = "Aspect"
    ) %>%
    addLayersControl(
      baseGroups = c("Colour"),
      # Add for each layer in my app
      overlayGroups = c("River", "Bridges", "Dams", "Large Wood", "Large Wood Catchers", "Heatmap", "Slope", "Aspect", "Bridge Risk Index"),
      options = layersControlOptions(collapsed=FALSE)
    ) %>%
    addMeasure(position = "topleft", primaryLengthUnit = "meters", activeColor = "green") %>%
    addLegend(
      position = "bottomright",
      pal = pal_clusters,
      values = lw_points$clusters,
      title = "LW Cluster ID",
      opacity = 0.8
    ) #%>%
    # addLegend(
    #   position = "bottomright",
    #   pal = pal_slope,
    #   values = values(slope),
    #   title = "Slope (Degrees)",
    #   opacity = 0.8
    # ) %>%
    # addLegend(
    #   position = "bottomright",
    #   pal = pal_aspect,
    #   values = values(aspect),
    #   title = "Aspect",
    #   opacity = 0.8
    # )
})

# Add popups for data
observe({
  leafletProxy("map") %>%
    addCircleMarkers(data = lw_points,
                     fillColor = ~pal_clusters(clusters),
                     color = "black",
                     weight = 1,
                     radius = 5,
                     stroke = TRUE,
                     fillOpacity = 0.7,
                     popup = ~paste("<b>Type:</b>", LW_Type,
                                    "<br><b>Imagery Used:</b>", satImg_src,
                                    "<br><b>Cluster ID:</b>", clusters),
                     group = "Large Wood") %>%
    addCircleMarkers(data = bridges_snapped,
                     color = "black",
                     weight = 1,
                     radius = 5,
                     stroke = TRUE,
                     fillOpacity = 0.7,
                     popup = ~paste("<b>Name:</b>", Name,
                                    "<br><b>Large Wood Upstream:</b>", LW_upstream,
                                    "<br><b>Mean Slope:</b>", mean_slope,
                                    "<br><b>Mean Aspect:</b>", mean_aspect),
                     group = "Bridges")
})

# Add interactive large wood plot to the plot1 panel
output$plot1 <- renderPlotly({
  p <- ggplot(lw_points, aes(x=LW_Type, fill=satImg_src)) +
    geom_bar(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Instream Large Wood Type by Imagery Source", x="", y="Count") +
    theme_minimal()
  ggplotly(p)
})

# Add dynamic plot to the floating panel on the map showing large wood by img source
# Based on the map view's current extents (bounds)
output$plot_dynamic <- renderPlotly({
  # If bounds not loaded, show full data coverage
  bounds <- input$map_bounds
  if (is.null(bounds)) {
    lw_filtered <- lw_points
  } else {
    # Filter large wood points by map extent bounds
    lw_filtered <- lw_points %>%
      filter(
        st_coordinates(.)[,1] > bounds$west,
        st_coordinates(.)[,1] < bounds$east,
        st_coordinates(.)[,2] > bounds$south,
        st_coordinates(.)[,2] < bounds$north,
      )
  }
  p <- ggplot(lw_filtered, aes(x=LW_Type, fill=satImg_src)) +
    geom_bar(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Instream Large Wood Types by Imagery Source in Current View", x="", y="Count") +
    theme_minimal()
  ggplotly(p)
})

# Boxplot (violin) for slope values
output$slope_plot <- renderPlotly({
  p <- ggplot(lw_points, aes(x = LW_Type, y = slope_val, fill = LW_Type)) +
    geom_violin() +
    labs(title = "Slope Distribution by Large Wood Type", y = "Slope (degrees)") +
    theme_minimal()
  ggplotly(p)
})

# Rose plot for aspect using summary table
output$aspect_plot<- renderPlotly({
  plot_ly(
    data = aspect_summary,
    theta = ~direction_bin,
    r = ~count,
    type = "barpolar",
    marker = list(color = "steelblue")
  ) %>%
    layout(title = "Aspect of Large Wood Locations",
           # Change polar axis to put North at the top
           polar = list(
             angularaxis = list(
               direction = "clockwise",
               rotation = 90
             )
           ),
           margin = list(t = 100))
})
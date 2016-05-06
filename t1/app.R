# load libraries ----
library(shiny)
library(rgdal)
library(dplyr)
library(jsonlite)
library(leaflet)
library(profvis) # devtools::install_github("rstudio/profvis")

# debug ----

# run profiling visualization
# P = profvis::profvis({shiny::runApp('./t1')}); print(P)

# set working directory if running from Console
# setwd('t1')

# load data ----

# rgdal
p_gdal <- readOGR('../data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.geojson', 'OGRGeoJSON', verbose = F)

# topojson
p_topo <- readLines('../data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.topojson.json', warn = FALSE) %>%
  jsonlite::fromJSON(simplifyVector=F)

# set color palette
pal <- colorNumeric(
  palette = 'RdYlBu',
  domain  = p_gdal@data$area_km2)

# user interface ----
ui <- shinyUI(fluidPage(
  leafletOutput('map_gdal'),
  leafletOutput('map_topo')
))

# server functions ----
server <- shinyServer(function(input, output) {
   
  output$map_gdal <- renderLeaflet({
  
    leaflet() %>%
      addProviderTiles('Stamen.TonerLite', options=tileOptions(noWrap=TRUE)) %>%
      setView(0,0,2) %>%
      addPolygons(
        data = p_gdal,
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = ~pal(area_km2)) %>%
      addLegend(
        "bottomright", pal = pal, opacity = 0.5,
        values = p_gdal@data$area_km2, title = 'p_gdal')
    
  })

  output$map_topo <- renderLeaflet({
      
    # default style for all features
    p_topo$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.7)
    
    # p_topo$objects[[1]]$geometries[[1]]$properties$style
    p_topo$objects[[1]]$geometries = lapply(p_topo$objects[[1]]$geometries, function(feat) { # feat = p_topo$objects[[1]]$geometries[[1]]
      rid = feat$properties$rgn_id
      feat$properties$style = list(
        fillColor = pal)
      feat })
    
    # Add the now-styled GeoJSON object to the map
    #leaflet() %>% addGeoJSON(geojson)
    
    # plot map
    leaflet() %>%
      addProviderTiles('Stamen.TonerLite', options=tileOptions(noWrap=TRUE)) %>%
      setView(0,0,2) %>%
      #addTopoJSON(p_topo, weight = 1, color = "#444444", fill = FALSE) %>%
      addTopoJSON(p_topo) %>%
      addLegend(
        "bottomright", pal = pal, opacity = 0.5,
        values = p_gdal@data$area_km2, title = 'p_topo')
  })
  
})
  
# Run the application 
shinyApp(ui = ui, server = server)


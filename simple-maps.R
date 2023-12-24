#
# Author: Andrew Disher
# Date: 12/22/2023
#
#

library(sf)
library(dplyr)
library(jsonlite)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(glue)

# ------------------------------------------
# ----- Function to Convert Data to SF -----
# ------------------------------------------

convert_to_SF <- function(API_data, geom_path = FALSE) {
  data_attr <- API_data$features$attributes
  data_geom <- API_data$features$geometry
  
  if(isFALSE(geom_path)){
    # Bind columns together and convert to sf data frame
    return_data <- cbind(data_attr, data_geom) %>% 
      st_as_sf(coords = c("x", "y"))
  } else if (isTRUE(geom_path)) {
    # Convert each 3d array into an sf LINESTRING object, together as a list
    for (path in 1:length(data_geom$paths)) {
      data_geom$paths[[path]] <- data_geom$paths[[path]][1, , ] %>% 
        as.matrix() %>% 
        st_linestring()
    }
    
    # Bind columns together and convert to sf data frame
    return_data <- data.frame(geometry = st_sfc(data_geom$paths)) %>% 
      cbind(data_attr, .) %>% 
      st_as_sf()
  }
  
  return(return_data)
}

# ---------------------------
# ----- Import the Data -----
# ---------------------------

# --- Rapid Transit ---
rapid_transit_stops <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/0/query?where=1%3D1&outFields=stop_id,stop_name,zone_id,stop_address,stop_url,wheelchair_boarding,municipality&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = FALSE)
rapid_transit_routes <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = TRUE)

# --- Commuter Rail ---
commuter_rail_stations <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/2/query?where=1%3D1&outFields=stop_id,stop_name,platform_name,zone_id,stop_url,parent_station,wheelchair_boarding,municipality,vehicle_type&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = FALSE)
commuter_rail_routes <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/3/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = TRUE)

# --- Bus ---
bus_stops <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/4/query?where=1%3D1&outFields=stop_url,wheelchair_boarding,municipality,vehicle_type,stop_id,stop_name&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = FALSE)
bus_routes <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/5/query?where=1%3D1&outFields=route_id,route_long_name,route_url,route_color,route_fare_class&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = TRUE)

# --- Ferry ---
ferry_stops <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/6/query?where=1%3D1&outFields=stop_name,stop_url,wheelchair_boarding,municipality,vehicle_type&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = FALSE)
ferry_routes <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/7/query?where=1%3D1&outFields=route_long_name,route_type,route_url,route_color,route_fare_class,route_id&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = TRUE)

# --- Other Stops and Routes ---
other_stops <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/10/query?where=1%3D1&outFields=stop_id,stop_name,stop_url,wheelchair_boarding,municipality&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = FALSE)
other_routes <- fromJSON("https://gis.massdot.state.ma.us/arcgis/rest/services/Multimodal/GTFS_Systemwide/MapServer/8/query?where=1%3D1&outFields=shape_id&outSR=4326&f=json") %>% 
  convert_to_SF(geom_path = TRUE)

# ----------------------------------------
# ----- Leaflet Map With Many Layers -----
# ----------------------------------------

mbta_icons <- iconList(
  rapidT = makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/MBTA.svg/2048px-MBTA.svg.png",
                    iconWidth = 15, iconHeight = 15, 
                    className = "rapid-icon"),
  bus = makeIcon(iconUrl = "https://png.pngtree.com/png-vector/20210124/ourmid/pngtree-bus-icon-design-template-vector-isolated-png-image_2801320.jpg",
                 iconWidth = 10, iconHeight = 10)
)

label_style = list("border-style" = "solid",
                   "border-width" = "3px",
                   "border-radius" = ".5em",
                   "font-size" = "16px",
                   "padding" = "5px")

line_weight <- 2

myMap <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 20)) %>% 
  addMapPane(name = "custom_layers", zIndex = 410) %>% 
  addMapPane(name = "rapid_transit_stops", zIndex = 415) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, 
                   options = leafletOptions(pane = "maplabels")) %>% 
  addPolylines(data = commuter_rail_routes, weight = line_weight, color = ~paste0("#", route_color),
               options = leafletOptions(pane = "custom_layers")) %>% 
  addPolylines(data = bus_routes, weight = line_weight, color = ~paste0("#", route_color),
               options = leafletOptions(pane = "custom_layers")) %>% 
  addPolylines(data = ferry_routes, weight = line_weight, color = "#008EAA", dashArray = "2 6",
               options = leafletOptions(pane = "custom_layers")) %>% 
  addPolylines(data = other_routes, weight = line_weight, color = "#00304d",
               options = leafletOptions(pane = "custom_layers")) %>% 
  addPolylines(data = rapid_transit_routes, weight = line_weight, color = ~paste0("#", route_color),
               options = leafletOptions(pane = "custom_layers")) %>% 
  addMarkers(data = rapid_transit_stops, icon = mbta_icons$rapidT, group = "rapid_transit_stops",
             options = leafletOptions(pane = "rapid_transit_stops"),
             label = glue("<strong>{rapid_transit_stops$stop_name}</strong> <br>
                          <strong>Address:</strong> {rapid_transit_stops$stop_address} <br>
                          <strong>Municipality:</strong> {rapid_transit_stops$municipality}") %>% lapply(HTML),
             labelOptions = labelOptions(
               style = c(label_style,
                         "border-color" = "black"))) %>% 
  addCircleMarkers(data = ferry_stops, radius = 3, color = "#008EAA", opacity = 1, fillOpacity = 1, 
             options = leafletOptions(pane = "custom_layers"),
             label = glue("<strong>{ferry_stops$stop_name}</strong> <br>
                          <strong>Municipality:</strong> {ferry_stops$municipality}") %>% lapply(HTML),
             labelOptions = labelOptions(
               style = c(label_style,
                         "border-color" = "#008EAA"))) %>% 
  addCircleMarkers(data = commuter_rail_stations, radius = 3, color = "#80276C", opacity = 1, fillOpacity = 1, 
             options = leafletOptions(pane = "custom_layers"),
             label = glue("<strong>{commuter_rail_stations$stop_name}</strong> <br>
                          <strong>Platform:</strong> {commuter_rail_stations$platform_name} <br>
                          <strong>Municipality:</strong> {commuter_rail_stations$municipality}") %>% lapply(HTML),
             labelOptions = labelOptions(
               style = c(label_style, 
                         "border-color" = "#80276C"))) %>% 
  addCircleMarkers(data = other_stops, radius = 3, color = "#00304d", opacity = 1, fillOpacity = 1, 
                   options = leafletOptions(pane = "custom_layers"),
                   label = glue("<strong>{other_stops$stop_name}</strong> <br>
                                <strong>Municipality:</strong> {other_stops$municipality}") %>% lapply(HTML),
                   labelOptions = labelOptions(
                     style = c(label_style, 
                               "border-color" = "#00304d")))
  # addMarkers(data = bus_stops, icon = mbta_icons$bus, group = "bus_stops")
  # addCircles(data = bus_stops, group = "bus_stops", radius = 1, opacity = .5, color = "#FFC72C")


myMap
  

  


  










# TO-DO:

  # htmlwidgets::onRender("
  #                       function(el, x) {
  #                         var myMap = this;
  #                         var busMarkerLayer = myMap.getLayers()[8];
  #                         myMap.on('zoomed',
  #                           function() {
  #                             console.log(myMap.getZoom());
  #                             if (myMap.getZoom() < 15 && myMap.hasLayer(myMap.getLayer('bus_stops'))) {
  #                               myMap.removeLayer(myMap.getLayer('bus_stops'));
  #                             }
  #                             if (myMap.getZoom() > 15 && myMap.hasLayer(myMap.getLayer('bus_stops')) == false) {
  #                               myMap.addLayer(myMap.getLayer('bus_stops'));
  #                             }
  #                         })
  #                       }")
  # htmlwidgets::onRender(
  #   "function(el, x) {
  #     let myMap = this;
  #     console.log(myMap);
  #     //console.log(myMap.layerManager.getLayer('rapid_transit_stops');
  #   }"
  # )









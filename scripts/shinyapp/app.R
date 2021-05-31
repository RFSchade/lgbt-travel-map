
#### LOAD PACKAGES AND DATA ####
pacman::p_load(pacman, shiny, leaflet, leatlet.extras, tidyverse)

# load in data
EEA_data <- st_read("data/EEA_points.shp")

# plot the points (don't know why it only shows 6)
#plot(st_geometry(EEA_data))

# look at the coordinates
#EEA_data["geometry"]

# check the crs
#st_crs(EEA_data)

# when looking at the first map (by running the next code chunk without the last bit of this chunk) the points were not at the right place - maybe 100 meters to the north east. I also got this error message:
#sf layer has inconsistent datum (+proj=longlat +ellps=intl +towgs84=-86,-98,-119,0,0,0,0 +no_defs).
#Need '+proj=longlat +datum=WGS84' 

# Therefore, I re-projected the test data to the crs recommended in the error message above, and it worked!
crs_needed <- "+proj=longlat +datum=WGS84"
safespace_EEA_crs <- st_transform(EEA_data, crs = crs_needed)



#### PREPARE THE BUFFERS FOR THE SAFE SPACES, TO BE USED TO FIND THE NEAREST SAFE SPACE LATER ####
r = 0.00001
safespace_EEA_crs$buffers <- st_buffer(safespace_EEA_crs$geometry, r)


#### UI ####

ui <- fluidPage(
  titlePanel("LGBTQ+ Travel Map"), 
  leafletOutput("map"),
)


#### SERVER ####

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addControl("Queer Travel Map", position = "topleft", className="map-title") %>% #
      addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      
      addCircleMarkers(data = safespace_EEA_crs,
                       opacity = 0.5, color = "black", stroke = TRUE,
                       fillOpacity = 0.5, weight=2, fillColor = "purple",
                       popup = paste0("Name: ", EEA_data$name,
                                      "<br> Type: ", EEA_data$amenity,
                                      "<br> Website: ", EEA_data$website,
                                      "<br> Opening hours: ", EEA_data$opnng_h), #,
                       group = "Safe spaces",
                       
                       clusterOptions = markerClusterOptions()) %>%
      
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en") %>% 
      
      addLayersControl(
        baseGroups = c("Topographic","Aerial"),
        overlayGroups = c("Safe spaces", "Gayborhoods"),
        options = layersControlOptions(collapsed = T)) %>% 
      
      addControlGPS(
        options = gpsOptions(position = "topleft", 
                             activate = TRUE, 
                             autoCenter = TRUE, maxZoom = 10, 
                             setView = TRUE))
  })
  
  
  observe({
    
    event <- input$map_gps_located
    if (is.null(event))
      return()
    
    GPS_buffer <- st_buffer(st_geometry(st_point(c(event$coordinates$lng, event$coordinates$lat))), r)
    st_crs(GPS_buffer) <- crs_needed
    
    safespace_EEA_crs$nearest_marker <- st_nearest_points(GPS_buffer, safespace_EEA_crs$buffers, pairwise = FALSE)
    
    safespace_EEA_crs$length <- st_length(safespace_EEA_crs$nearest_marker)

    lines_lengths_sorted <- safespace_EEA_crs[order(safespace_EEA_crs$length),]
    
    nearest <- lines_lengths_sorted$nearest_marker[1,]
    nearest_name <- lines_lengths_sorted$name[1]
    nearest_length <- lines_lengths_sorted$length[1]
    nearest_safe <- lines_lengths_sorted$geometry[1]
    nearest_type <- lines_lengths_sorted$amenity[1]
    nearest_website <- lines_lengths_sorted$website[1]
    nearest_open <- lines_lengths_sorted$opnng_h[1]
    
    print(nearest_name)
    print(nearest)
    
    leafletProxy("map") %>% clearPopups() %>% 
      addMarkers(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat,
        popup = paste0("You are here! Follow the blue line to see your nearest safe space.
               <br> The safe space is called ", nearest_name,
                       "<br> It is ", round(nearest_length/1000, 1), " km away."),
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>% 
      
      addPopups(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat, 
        popup = paste0("You are here! Follow the blue line to see your nearest safe space.
               <br> The safe space is called ", nearest_name,
                       "<br> It is ", round(nearest_length/1000, 1), " km away.")) %>%

      addPopups(
        data = nearest_safe, 
        popup = paste0("Name: ", nearest_name,
                       "<br> Type: ", nearest_type,
                       "<br> Website: ", nearest_website,
                       "<br> Opening hours: ", nearest_open)
      ) %>% 
      addPolygons(data = nearest)
  })
  
}

shinyApp(ui,server)
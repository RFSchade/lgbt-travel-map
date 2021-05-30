
pacman::p_load(pacman, shiny, leaflet, leatlet.extras, tidyverse)

ui <- fluidPage(
  titlePanel("LGBTQ+ Travel Map"), 
  leafletOutput("map"))


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
      
      
      #addEasyButton(
       # easyButton(
        #  position = "topleft",
         # icon = "fa-crosshairs",
          #title = "Locate Me",
          #onClick = JS("
           # function(btn, map) {
            #  Shiny.onInputChange('my_easy_button', 'clicked');
            #}"
                       
          #)
        #)
      #) %>% 
      
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en") %>% 
      
      addLayersControl(
        baseGroups = c("Topographic","Aerial"),
        overlayGroups = c("Safe spaces", "Gayborhoods", "My location"),
        options = layersControlOptions(collapsed = T)) %>% 
      
      addControlGPS(
        options = gpsOptions(position = "topleft", 
                             activate = TRUE, 
                             autoCenter = TRUE, maxZoom = 10, 
                             setView = TRUE))
  })
  
  #observe(
   # print(paste0("map center - lat: ", input$map_gps_located$coordinates$lat, ", lon: ", input$map_gps_located$coordinates$lng))
  #)
  
  observe({
    
    event <- input$map_gps_located
    if (is.null(event))
      return()
    
    leafletProxy("map") %>% clearPopups() %>% 
      addMarkers(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat) %>% 
      addPopups(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat, "Here are you")
  })
  
}

shinyApp(ui,server)

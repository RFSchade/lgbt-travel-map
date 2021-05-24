
pacman::p_load(pacman, shiny, leaflet, tidyverse)

#https://www.rdocumentation.org/packages/leaflet/versions/2.0.4.1/topics/leafletOutput
#UI is the user interface. the "map" is the outputId, which is the "output variable to read from"
ui <- fluidPage(titlePanel("LGBTQ+ Travel Map"), leafletOutput("map")) 


server <- function(input, output, session) {
  
  center_coor <- c()
  
  # get the coordinates from the center and print them
  observeEvent(input$map_bounds, {
    event <- input$map_bounds
    
    lat <- mean(event$north, event$south)
    lon <- mean(event$west, event$east)
    
    center_coor <- c(lat, lon) # should it be lon, lat, or lat, lon??
    print(paste0("map center - lat: ", lat, ", lon: ", lon))
    #print(center_coor)
    print(center_coor)
  })
  
  
  
  
  # map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% # insert this in the parenthesis: attribution = 'By Rebecca Schade and Sophia Karlson'
      addControl("Queer Travel Map", position = "topleft", className="map-title") %>% # make this bigger!
      addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      
      # add center marker
      #addMarkers(data = center_coor) %>% #lng = lon, lat = lat) %>% 
      
      addCircleMarkers(data = safespace_crs,
                       opacity = 0.5, color = "black", stroke = TRUE,
                       fillOpacity = 0.5, weight=2, fillColor = "purple",
                       popup = paste0("Name: ", test_data$name,
                                      "<br> Type: ", test_data$amenity,
                                      "<br> Website: ", test_data$website,
                                      "<br> Opening hours: ", test_data$opnng_h), #,
                       # if we had the data, these could be added as well:
                       #"<br> Address: ", test_data$address),
                       # who it is for (only men, everyone etc)
                       group = "Safe spaces",#if we don't want the user to be able to not see all the safe spaces, cut this
                       #) %>%
                       clusterOptions = markerClusterOptions()) %>%
      
      # add density overlay here - e.g. polygons like here:
      addPolygons(data = safe_ch,
                  fill = T, weight = 2, color = "purple",
                  popup = paste0("Name: ", "gayboorhood-name"), 
                  group = "Safe space density") %>%
      
      # do the control stuff
      addLayersControl(
        baseGroups = c("Topographic","Aerial"),
        overlayGroups = c("Safe spaces", "Safe space density"),
        options = layersControlOptions(collapsed = T)) %>% 
      
      #leaflet() %>%
      # addProviderTiles(providers$Stamen.TonerLite,
      #                 options = providerTileOptions(noWrap = TRUE)) %>%
      
      addEasyButton(
        easyButton(
          position = "topleft",
          icon = "fa-crosshairs",
          title = "Locate Me",
          onClick = JS(
            c(
              "function(btn,  map){map.locate({setView:true,enableHighAccuracy: true })}"
            )
          )
        )
      ) %>% 
      
      addMarkers(
        lng = coor$Longitude, 
        lat = coor$Latitude,
        popup = paste("You are here!")) %>% 
      
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en")
    
  })
  
}

shinyApp(ui,server)

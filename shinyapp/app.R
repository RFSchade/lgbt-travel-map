### Title: LGBTQ+ Travel Map 
### Author: Sophia Kleist Karlson
### Date and place: June 2021, Aarhus University, Denmark


#### To run the app from the console, paste this into the console: 
#install.packages("shiny")
#library(shiny)
#runApp("shinyapp")

# uncomment the below to run locally in RStudio on Sophia's computer and not in console as above
# if un locally on another computer, the working directory should be inside the "shinyapp" folder
#setwd("~/Spatial analytics/project - queer travel map/lgbt-travel-map/shinyapp")


#### LOAD PACKAGES AND DATA ####

library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(stringr)
library(lwgeom)

# load in data
EEA_data <- st_read("../data/EEA_points.shp") 
#### NB: for sending the script to shinyapps.io (for getting a link to the app),
#### you have to put the data-folder into the shinyapp-folder and delete the "../" from the path

# plot the points (don't know why it only shows 6)
#plot(st_geometry(EEA_data))

# look at the coordinates
#EEA_data["geometry"]

# check the crs
#st_crs(EEA_data)

# when looking at the initial map (by running a leaflet map with the data) 
#the points were not at the right place - maybe 100 meters to the north east. I also got this error message:
#sf layer has inconsistent datum (+proj=longlat +ellps=intl +towgs84=-86,-98,-119,0,0,0,0 +no_defs).
#Need '+proj=longlat +datum=WGS84' 

# I can also read here https://rstudio.github.io/leaflet/projections.html that WGS84 is the default projection of Leaflet 
# -> therefore, I re-project the data to this crs and it works!
crs_needed <- "+proj=longlat +datum=WGS84"
safespace_EEA_crs <- st_transform(EEA_data, crs = crs_needed)




#### PREPARING SAFE SPACE HULLS/GAYBORHOODS ####

gayborhoods <- st_read("../data/gayborhoods1km.shp") # load in data
#### NB: for sending the script to shinyapps.io (for getting a link to the app),
#### you have to put the data-folder into the shinyapp-folder and delete the "../" from the path

gayborhoods <- st_transform(gayborhoods, crs = crs_needed) # transform to the needed crs





#### UI ####

# prepare the user interface
ui <- fluidPage(
  titlePanel("LGBTQ+ Travel Map"), # map title
  titlePanel(h5("Creators: Rebecca Folmer Schade and Sophia Kleist Karlson")), # map subtitle
  leafletOutput("map"), # the output should be a leaflet map
  column( # add a column below the map to explain the map
    12,
    h5("Wellcome to our LGBTQ+ Travel Map! On this map, you can see LGBTQ+ safe spaces all over Europe.
       The safe spaces and gayborhoods are hiding within the colored bubbles, so click on a bubble to zoom in on the area you want to check out!
       The safe spaces are marked with purple circles, and gayborhoods with purple polygons. Click on a safe space or gayborhood to see information about it. 
       If you click the crosshair button on the left, you will see your own location (red marker) and your nearest safe space (purple marker).
       If you click the measurement button on the left, you can plan a route and see its length.
       Finally, in the top right corner, you can choose if you want to see a topographic or aerial map. Here, you can also choose to see only safe spaces, or only gayborhoods.
       WARNING: The gayborhoods highlight clusters of safe spaces, but they do not guarantee that the area between those safe spaces will be safe!
       This map is part of an exam project for the course Spatial Analytics at Aarhus University, Denmark.")
  ))




#### SERVER ####

# now the server function is defined
server <- function(input, output) {
  
  output$map <- renderLeaflet({ # the output is a leaflet map
    
    leaflet() %>%
      addTiles() %>%
      
      # we add both topographical and aerial tiles
      addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      
      # now add circle markers of the safe spaces
      addCircleMarkers(data = safespace_EEA_crs,
                       # marker layout
                       opacity = 0.5, color = "black", stroke = TRUE,
                       fillOpacity = 0.5, weight=2, fillColor = "purple",
                       
                       # add pop-ups for each marker, showing the name, type (is it a bar, restaurant, etc.), website and opening hours, if this information is available
                       popup = paste0("Name: ", safespace_EEA_crs$name,
                                      "<br> Type: ", safespace_EEA_crs$amenity,
                                      "<br> Website: ", safespace_EEA_crs$website,
                                      "<br> Opening hours: ", safespace_EEA_crs$opnng_h),
                       
                       # add the markers to the safe space group
                       group = "Safe spaces",
                       
                       # add the cluster option, so the map doesn't get too cramped
                       clusterOptions = markerClusterOptions()) %>%
      
      
      # add the gayborhood overlay
      addPolygons(data = gayborhoods,
                  fill = T, weight = 2, color = "purple",
                  popup = paste0("This is a gayborhood! It's area is ", 
                                 round(gayborhoods$area_km2, 2),
                                 #round(st_area(gayborhoods$geometry), 2),
                                 " km² and it has ", gayborhoods$nr_points, " safe spaces."),
                  group = "Gayborhoods") %>%
      
      
      # add a control panel to control the tiles and groups
      addLayersControl(
        baseGroups = c("Topographic","Aerial"),
        overlayGroups = c("Safe spaces", "Gayborhoods"),
        options = layersControlOptions(collapsed = F)) %>% 
      
      # add the control GPS feature, to get the location of the user
      addControlGPS(
        options = gpsOptions(position = "topleft", 
                             activate = TRUE, 
                             autoCenter = TRUE, maxZoom = 10, 
                             setView = TRUE)) %>% 
      
      # add the measure feature, so the user can plan a route and get it's length
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en")
  })
  
  # now we use the observe() function outside the rendered leaflet map, 
  # in order to capture the user's location, if that button has been pressed, 
  # and use the coordinates to find the nearest safe space
  observe({
    
    event <- input$map_gps_located # the input$map_gps_located stores the user's location if the button has been pressed
    if (is.null(event)) # if the event doesn't exist, ignore the rest
      return()
    
    # now we create point on the map from the user's location coordinates
    gps_point <- st_geometry(st_point(c(event$coordinates$lng, event$coordinates$lat)))
    st_crs(gps_point) <- crs_needed # make sure it has the same crs as the rest of the safe spaces
    
    # add a column to the safespace_EEA_crs object, containing the linestrings between the user location and each of the safe spaces
    # to get the linestrings, I use the st_nearest_points() function
    #safespace_EEA_crs$nearest_marker <- st_nearest_points(GPS_buffer, safespace_EEA_crs$buffers, pairwise = FALSE)
    
    safespace_EEA_crs$nearest_marker_lines <- st_nearest_points(gps_point, safespace_EEA_crs$geometry, pairwise = FALSE)
    
    # add the length of each linestring to safespace_EEA_crs using the st_length() function
    safespace_EEA_crs$length <- st_length(safespace_EEA_crs$nearest_marker_lines)

    # sort safespace_EEA_crs by the length column and save as a new object (lines_lengths_sorted)
    lines_lengths_sorted <- safespace_EEA_crs[order(safespace_EEA_crs$length),]
    
    # now I define the needed variables from the lines_lengths_sorted
    # as the object is sorted from the lowest length to highest, I use the first for each variable
    nearest_line <- lines_lengths_sorted$nearest_marker_lines[1,] # the linestring between user location and the nearest safe space
    nearest_name <- lines_lengths_sorted$name[1] # the name of the nearest safe space
    nearest_length <- lines_lengths_sorted$length[1] # the distance (length of linestring) to the nearest safe space
    nearest_safe <- lines_lengths_sorted$geometry[1] # the coordinates/geometry of the nearest safe space
    nearest_type <- lines_lengths_sorted$amenity[1] # the type of the nearest safe space (restaurant, bar, etc.)
    nearest_website <- lines_lengths_sorted$website[1] # the website of the nearest safe space
    nearest_open <- lines_lengths_sorted$opnng_h[1] #the opening hours of the nearest safe space
    
    
    # now we are ready to add the stuff that happens when the "get location" button is pressed
    # first, we define some marker icons for the nearest safe space feature
    icon_location <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'red'
    )
    
    icon_nearest <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'purple'
    )
    
    # then we make a leaflet Proxy map, so that the whole map doesn't have to be rendered again
    leafletProxy("map") %>% clearPopups() %>% 
      
      # add a marker to the nearest safe space
      addAwesomeMarkers(
        data = nearest_safe, 
        icon = icon_nearest,
        
        # pop-up with info
        popup = paste0("This is your nearest safe space. Zoom in and follow the blue line to see its exact location.
                        <br> It is ", round(nearest_length/1000, 1), " km away from your position.
                        <br> Name: ", nearest_name,
                       "<br> Type: ", nearest_type,
                       "<br> Website: ", nearest_website,
                       "<br> Opening hours: ", nearest_open),
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>%
      
      # also add an extra pop-up to the nearest marker, which will open automatically (which the marker pop-up doesn't)
      addPopups(
        data = nearest_safe, 
        popup = paste0("This is your nearest safe space. Zoom in and follow the blue line to see its exact location.
                        <br> It is ", round(nearest_length/1000, 1), " km away from your position.
                        <br> Name: ", nearest_name,
                       "<br> Type: ", nearest_type,
                       "<br> Website: ", nearest_website,
                       "<br> Opening hours: ", nearest_open)) %>%
      
      # add a marker to the location of the user 
      addAwesomeMarkers(
        lng = event$coordinates$lng, #the event object contains the longitude and latitude of the user 
        lat = event$coordinates$lat,
        
        # add pop-up
        popup = "You are here!",
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),# so the pop-up doesn't disapear if the user clicks on the map
        icon = icon_location) %>% 
      
      # again, add an extra pop-up to the user location, which will open automatically (which the marker pop-up doesn't)
      addPopups(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat, 
        popup = "You are here!") %>% 
      
      # add the line from the user's location to the nearest marker, using the linestring from above
      addPolygons(data = nearest_line, color = "purple")
  })
  
}



#### RUN THE APP! ####

shinyApp(ui,server)

#sessionInfo()
#RStudio.Version()
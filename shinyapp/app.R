#### To run the script, paste this into the console: 
#library(shiny)
#runApp("shinyapp")

#uncomment the below to run in RStudio and not in the way above
#setwd("~/Spatial analytics/project - queer travel map/lgbt-travel-map/shinyapp")

#### LOAD PACKAGES AND DATA ####
pacman::p_load(pacman, shiny, sf, leaflet, leaflet.extras, tidyverse, stringr, htmlwidgets, dplyr)
#pacman, tidyverse, dplyr, sf, leaflet, stringr, htmlwidgets, raster, shiny
# load in data

EEA_data <- st_read("data/EEA_points.shp")#../

# plot the points (don't know why it only shows 6)
#plot(st_geometry(EEA_data))

# look at the coordinates
#EEA_data["geometry"]




#### Safe space hulls data:
# Checking data 
#head(test_data)

# re-projecting data because I am a dum dum 
test_transform <- st_transform(EEA_data, crs = 3035)
head(test_transform)
# > Perfect! The unit of this is in metres! 
plot(st_geometry(test_transform))

# Making a coordinate dataframe with lat and long in separate columns
test_transform_df <- st_coordinates(test_transform)
colnames(test_transform_df) <- c('northing', 'easting')

# Making clusters
# > Units should be in metres, so this should be 5 km radius
res <-  dbscan(test_transform_df, eps = 5000, minPts = 3)
res

# Giving datapoints cluster ID's 
test_transform$dbscan_id <- res$cluster

# Making a dataset of clusters
clusters <- test_transform %>%
  filter(dbscan_id >= 1) %>% 
  group_by(dbscan_id) %>% 
  summarize(geometry = st_union(geometry), nr_points = n())

# Computing the convex hull 
cluster_hull <- st_convex_hull(clusters)

# Ploting the points together with the hull
plot(st_geometry(test_transform));plot(cluster_hull, add = TRUE) 
# > This is not a very good visualization, but I think I get it. 

#=====> Maybe it will help if I try to visualize it in leaflet 
# Transforming to web mercator 
crs_needed <- "+proj=longlat +datum=WGS84"
test_web <- st_transform(test_transform, crs = crs_needed)
hull_web<- st_transform(cluster_hull, crs = crs_needed)




# check the crs
#st_crs(EEA_data)

# when looking at the initial map (by running a leaflet map with the data) 
#the points were not at the right place - maybe 100 meters to the north east. I also got this error message:
#sf layer has inconsistent datum (+proj=longlat +ellps=intl +towgs84=-86,-98,-119,0,0,0,0 +no_defs).
#Need '+proj=longlat +datum=WGS84' 

# Therefore, I re-projected the test data to the crs recommended in the error message above, and it worked!
crs_needed <- "+proj=longlat +datum=WGS84"
safespace_EEA_crs <- st_transform(EEA_data, crs = crs_needed)



#### PREPARE THE BUFFERS FOR THE SAFE SPACES, TO BE USED TO FIND THE NEAREST SAFE SPACE LATER ####
r = 0.00001
safespace_EEA_crs$buffers <- st_buffer(safespace_EEA_crs$geometry, r)


#### UI ####

# prepare the user interface
ui <- fluidPage(
  titlePanel("LGBTQ+ Travel Map"), # map title
  leafletOutput("map"), # the output should be a leaflet map
  column( # add a column below the map to explain the map
    10,
    h5("Wellcome to our LGBTQ+ Travel Map! On this map, you can see safe spaces all over Europe.
       The safe spaces are clustered within the colored bubbles, so click on a bubble to zoom in on the area you want to check out!
       The safe spaces are marked with purple circles. Click on a circle to see information about the safe space. 
       If you click the crosshair button on the left, you will see your own location and your nearest safe space.
       If you click the measurement button on the left, you can plan a route and see it's length.
       Finally, in the top right corner, you can choose if you want to see a topographic or aerial map. Here, you can also choose to see only safe spaces, or only gayborhoods.
       Creators: Rebecca Folmer Schade and Sophia Kleist Karlson.")
  ))


#### SERVER ####

# now the server function is defined
server <- function(input, output, session) {
  
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
      
      
      # add overlay - e.g. polygon like here:
      addPolygons(data = hull_web,
                  fill = T, weight = 2, color = "purple",
                  popup = "This is a gayborhood!", #paste0("Name: ", "gayboorhood-name"), 
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
    
    # now we create a buffer zone around the user's location, 
    #with the same radius as around each safe space (as defined in the top of the script)
    GPS_buffer <- st_buffer(st_geometry(st_point(c(event$coordinates$lng, event$coordinates$lat))), r)
    st_crs(GPS_buffer) <- crs_needed # make sure it has the same crs as the rest of the safe spaces
    
    # add a column to the safespace_EEA_crs object, containing the linestrings between the user location and each of the safe spaces
    # to get the linestrings, I use the st_nearest_points() function
    safespace_EEA_crs$nearest_marker <- st_nearest_points(GPS_buffer, safespace_EEA_crs$buffers, pairwise = FALSE)
    
    # add the length of each linestring to safespace_EEA_crs using the st_length() function
    safespace_EEA_crs$length <- st_length(safespace_EEA_crs$nearest_marker)

    # sort safespace_EEA_crs by the length column and save as a new object (lines_lengths_sorted)
    lines_lengths_sorted <- safespace_EEA_crs[order(safespace_EEA_crs$length),]
    
    # now I define the needed variables from the lines_lengths_sorted
    # as the object is sorted from the lowest length to highest, I use the first for each variable
    nearest_line <- lines_lengths_sorted$nearest_marker[1,] # the linestring between user location and the nearest safe space
    nearest_name <- lines_lengths_sorted$name[1] # the name of the nearest safe space
    nearest_length <- lines_lengths_sorted$length[1] # the distance (length of linestring) to the nearest safe space
    nearest_safe <- lines_lengths_sorted$geometry[1] # the coordinates/geometry of the nearest safe space
    nearest_type <- lines_lengths_sorted$amenity[1] # the type of the nearest safe space (restaurant, bar, etc.)
    nearest_website <- lines_lengths_sorted$website[1] # the website of the nearest safe space
    nearest_open <- lines_lengths_sorted$opnng_h[1] #the opening hours of the nearest safe space
    
    
    # now we are ready to add the 
    leafletProxy("map") %>% clearPopups() %>% 
      addMarkers(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat,
        popup = "You are here!",
      # popup = paste0("You are here! Zoom in and follow the blue line to see your nearest safe space.
      #       <br> The safe space is ", round(nearest_length/1000, 1), " km away.
      #      <br> Name: ", nearest_name,
      #    "<br> Type: ", nearest_type,
      #   "<br> Website: ", nearest_website,
      #  "<br> Opening hours: ", nearest_open)) %>%
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>% 
      
      addPopups(
        lng = event$coordinates$lng,
        lat = event$coordinates$lat, 
        popup = "You are here!") %>% 
       # popup = paste0("You are here! Zoom in and follow the blue line to see your nearest safe space.
        #       <br> The safe space is ", round(nearest_length/1000, 1), " km away.
         #      <br> Name: ", nearest_name,
          #    "<br> Type: ", nearest_type,
           #   "<br> Website: ", nearest_website,
            #  "<br> Opening hours: ", nearest_open)) %>%

      addMarkers(
        data = nearest_safe, 
        popup = paste0("This is your nearest safe space. Zoom in and follow the blue line to see the exact location.
      <br> It is ", round(nearest_length/1000, 1), " km away from your position.
    <br> Name: ", nearest_name,
              "<br> Type: ", nearest_type,
              "<br> Website: ", nearest_website,
              "<br> Opening hours: ", nearest_open),
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)) %>%
      
      addPopups(
        data = nearest_safe, 
        popup = paste0("This is your nearest safe space. Zoom in and follow the blue line to see the exact location.
      <br> It is ", round(nearest_length/1000, 1), " km away from your position.
    <br> Name: ", nearest_name,
                       "<br> Type: ", nearest_type,
                       "<br> Website: ", nearest_website,
                       "<br> Opening hours: ", nearest_open)) %>%
      
      addPolygons(data = nearest_line)
  })
  
}

shinyApp(ui,server)

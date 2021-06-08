# LGBTQ+ Travel Map
Creators: Rebecca Folmer Schade and Sophia Kleist Karlson.

This repository contains the code and data for our LGBTQ+ Travel Map: an interactive map made to help LGBTQ+ people planning a safe journey. The map is made as an R shiny app and is part of an exam project for the course Spatial Analytics at Aarhus University, Denmark. 

### App description
The map and its interactive features are described below. You can find images showing these features in the “figures” folder within this repository (the images are called “1A”-“1I”):

When running and opening the app in your browser, you will see colorful bubbles spread over Europe, as well as some smaller purple circles and polygons. The latter two are the safe spaces and gayborhoods, most of which are hiding within the colorful bubbles (which represent clusters of safe spaces), so click on a bubble to zoom in on the area you want to check out. Now click on a safe space or gayborhood to see information about it. If you click the crosshair button on the left, you will see your own location (red marker) and your nearest safe space (purple marker). If you click the measurement button on the left, you can plan a route and see its length. Finally, in the top right corner, you can choose if you want to see a topographic or aerial map. Here, you can also choose to see only safe spaces, or only gayborhoods.

WARNING: The gayborhoods highlight clusters of safe spaces, but they do not guarantee that the area between those safe spaces will be safe. 

### Running the app
To access and interact with the map, clone the repository, make sure to meet the required software and package versions (as specified in the project report), and run the following code in the RStudio console: 

install.packages("shiny") 

library(shiny)

runApp(“shinyapp”)

This will run the app.R script and initialize the app. Alternatively, the you can run through the app.R script manually - here you have to be careful that the working directory is set to the "shinyapp" folder. 

### Reproducing data extraction and cleaning scripts:
The data extraction and cleaning scripts are found in the "scripts" folder. To reproduce the scripts, simply clone the repository, make sure to meet the required software and package versions (as specified in the project report), and set your working directory to the main folder of the project, before running the scripts.

### Meta Data: 

### Legal Software License:

# LGBTQ+ Travel Map
Creators: Rebecca Folmer Schade and Sophia Kleist Karlson.

This repository contains the code and data for our LGBTQ+ Travel Map: an interactive map made to help LGBTQ+ people planning a safe journey. The map is made as an R shiny app and is part of an exam project for the course Spatial Analytics at Aarhus University, Denmark. 

Try the app here! https://sophia-kleist-karlson.shinyapps.io/shinyapp/

### Quick overview of the repository:
This repository contains 4 folders: "data" contains the raw and cleaned data used for the project. "figures" has screenshots taken of the app in action. In "scripts" you will find the scripts for data extraction and cleaning, creating gayborhoods and making descriptive statistics. Finally, "shinyapp" contains the app.R script that will run the shiny app. 

### App description
The map and its interactive features are described below. You can find images showing these features in the “figures” folder within this repository (the images are called “1A”-“1I”):

When running and opening the app in your browser, you will see colorful bubbles spread over Europe, as well as some smaller purple circles and polygons. The latter two are the safe spaces and gayborhoods, most of which are hiding within the colorful bubbles (which represent clusters of safe spaces), so click on a bubble to zoom in on the area you want to check out. Now click on a safe space or gayborhood to see information about it. If you click the crosshair button on the left, you will see your own location (red marker) and your nearest safe space (purple marker). If you click the measurement button on the left, you can plan a route and see its length. Finally, in the top right corner, you can choose if you want to see a topographic or aerial map. Here, you can also choose to see only safe spaces, or only gayborhoods.

WARNING: The gayborhoods highlight clusters of safe spaces, but they do not guarantee that the area between those safe spaces will be safe. 

### Running the app
To access and interact with the map, simply follow this link: https://sophia-kleist-karlson.shinyapps.io/shinyapp/

If you want to open the app from RStudio, you should clone the repository, make sure to meet the required software and package versions (as specified in the project report), and run the following code in the RStudio console: 

install.packages("shiny") 

library(shiny)

runApp(“shinyapp”)

This will run the app.R script and initialize the app. Alternatively, the you can run through the app.R script manually - here you have to be careful that the working directory is set to the "shinyapp" folder. 

NB: As is specified in the app.R and shinyapp_io_script.R scripts, the process of hosting the app on shinyapps.io (which gives us the link to the app), is tied to a personal account on https://www.shinyapps.io/ and can therefore not be reproduced, as the personal information in shinyapp_io_script.R has been deleted from the script here on GitHub. The app.R script here on GitHub is ready to be run in R as described above.

### Reproducing scripts:
To reproduce the scripts in the "scripts" folder, simply clone the repository, make sure to meet the required software and package versions (as specified in the project report), and set your working directory to the main folder, before running the scripts. 

NB: It should be added that even though our scripts are open source, the data is not, as it origins in **Eurostat data** which does not have an open source license. 

### Metadata description:
See the lgbtq_travel_map_data_package.json file in the "data" folder for metadata on the data used in the app. **The folder "ne_10m_admin_0_sovereignty" inside the data-folder contains data fron Natural Earth, which is used for data cleaning.**

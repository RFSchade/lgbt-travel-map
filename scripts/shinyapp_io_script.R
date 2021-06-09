### Title: shinyapps.io connection script
### Author: Sophia Kleist Karlson
### Date: June 9, 2021

### This script connects the R shiny app script to the online server shinyapps.io, 
### which makes the app into a link, so we don't have to run the script to initialize the app every time

install.packages('rsconnect') # install rsconnect
library(rsconnect) # load package

# set the account info
rsconnect::setAccountInfo(name='sophia-kleist-karlson',
                # I have deleted token and secret strings below, as they show personal information:
                          token='token', 
                          secret='secret')

# connect the app script to the account
rsconnect::deployApp("~/Spatial analytics/project - queer travel map/lgbt-travel-map/shinyapp")
Y # answer yes, if you want to update the app.R script

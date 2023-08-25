library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)

source('config.R')
source('functions.R')


## Test NMEA (if applicable)
testNMEA(settings = settings)

# Run App
shiny::runApp(host = '0.0.0.0', port = 80)

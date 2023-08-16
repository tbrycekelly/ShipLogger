library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)

source('config.R')
source('functions.R')

# Run App
runApp(port = 80, host = "0.0.0.0")

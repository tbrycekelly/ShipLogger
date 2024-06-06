library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(SimpleMapper)

source('config.R')
source('functions.R')

rstudioapi::jobRunScript('nmea.R')

# Run App
shiny::runApp(host = '0.0.0.0', port = 80)

library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(openxlsx)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(SimpleMapper)
library(SimpleBathy)

source('config.R')
source('functions.R')

#system('python3 ./message_logger.py', intern = F, wait = F)

# Run App
shiny::runApp(host = '0.0.0.0', port = 80)

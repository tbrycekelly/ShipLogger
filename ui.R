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

source('config.R')
source('functions.R')

ui = dashboardPage(
  header = dashboardHeader(title = 'ShipLogger'),
  title = 'ShipLogger',
  skin = 'black',

  sidebar = dashboardSidebar(
    width = 350,
    h3("Current time: "),
    h4(textOutput("currentTime", container = span)),
    h4(textOutput("currentTime.local", container = span)),
    h4(textOutput("lat", container = span)),
    h4(textOutput("lon", container = span)),
    h4(textOutput("age", container = span)),
    br(),
    h3('Export:'),
    downloadButton('download.csv', label = 'CSV'),
    downloadButton('download.xlsx', label = 'XLSX'),
    downloadButton('download.json', label = 'JSON'),
    downloadButton('download.pos', label = 'Positions'),
    hr(),
    br(),br(),
    actionButton('about', 'About'),
    actionButton("exit","Shutdown")
  ),

  body = dashboardBody(
    box(title = 'Entry Queuing',
        column(
          width = 6,
          shiny::textInput("stn", label = 'Station', width = '10em'),
          shiny::textInput("cast", label = 'Cast', width = '10em'),
        ),
        column(
          width = 6,
          shiny::selectInput("instrument",
                             label = 'Instrument',
                             choices = instruments,
                             width = '35em'),
          shiny::selectInput("author",
                             label = 'Author',
                             choices = authors,
                             width = '35em'),
          textInput("notes", label = 'Entry Comments',
                    width = '35em'),

          actionButton('queue', label = 'Queue Event'),
          actionButton('clear.enter', label = 'Reset')
        )
    ),

    ## Dataframe
    box(
      height = 800,
      width = 12,
      title = 'Event Log',
      actionButton("edit_button", "Details/Edit", icon("edit")),
      actionButton('refresh', label = 'Refresh', icon = icon('arrows-rotate')),
      actionButton("delete_button", "Delete", icon("trash-alt")),
      br(),br(),
      DT::dataTableOutput("events", fill = T, height = 450)
    )
  )
)


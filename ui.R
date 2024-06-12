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
    plotOutput('cruisemap'),
    sliderInput('scale', 'Map Zoom', min = 0, max = 10, step = 0.5, value = 4),
    h3('Export Options'),
    div(
      downloadButton('download.csv', label = 'CSV', class = 'export'),
      downloadButton('download.xlsx', label = 'XLSX', class = 'export'),
      downloadButton('download.json', label = 'JSON', class = 'export'),
      downloadButton('download.pos', label = 'Positions', class = 'export')
    ),
    hr(),
    br(),br(),
    div(
    actionButton('about', 'About'),
    actionButton("exit","Shutdown")
    )
  ),

  body = dashboardBody(
    tags$script(inactivity),
    box(title = 'Entry Queuing', width = 12,
        column(
          width = 2,
          shiny::textInput("stn", label = 'Station ID', width = '10em'),
          shiny::textInput("cast", label = 'Cast Number', width = '10em'),
        ),
        column(
          width = 4,
          shiny::selectInput("instrument",
                             label = 'Instrument',
                             choices = names(instruments),
                             width = '35em'),
          shiny::selectInput("author",
                             label = 'Author/Event Owner',
                             choices = authors,
                             width = '35em')
        ),
        column(
          width = 3,
          textAreaInput("notes", label = 'Entry Comments',
                    width = '45em', height = '5em'),
        ),
        column(
          width = 3,
          textInput('depth', label = 'Bottom Depth (m)', width = '10em'),
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


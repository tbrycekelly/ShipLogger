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

  sidebar = dashboardSidebar(minified = F,
    width = 400,
    h5("Current time: "),
    h4(textOutput("currentTime", container = span)),
    h4(textOutput("currentTime.local", container = span)),
    h4(uiOutput("age", container = span)),
    hr(),
    h4(uiOutput("lat", container = span)),
    h4(uiOutput("lon", container = span)),
    br(),
    plotOutput('cruisemap'),
    sliderInput('scale', 'Map Zoom', min = 0, max = 8, step = 0.25, value = 4),
    sliderInput('days', 'Map Duration (days)', min = 1, max = 60, step = 1, value = 60),
    hr(),
    h5('Export Options'),
    div(
      downloadButton('download', label = 'Database', class = 'export', width = '25%'),
      downloadButton('downloadXLSX', label = 'XLSX', class = 'export', width = '25%'),
      downloadButton('downloadJSON', label = 'JSON', class = 'export', width = '25%'),
      downloadButton('downloadPositions', label = 'Positions', class = 'export', width = '25%')
    ),
    hr(),
    span(
      actionButton('about', 'About', class = 'export', width = '80px'),
      actionButton("exit","Shutdown", class = 'export', width = '80px')
    )
  ),

  body = dashboardBody(
    tags$head(
      tags$style(
        HTML("
      .nav-tabs { display: none; }

      .data-label {
        font-weight: 600;
        color: #444;
        margin-right: 8px;
      }

      hr {
      border-top: 1px solid #ccc;
      }

      .data-value {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 2px 6px;
        display: inline-block;
        min-width: 60px;
        text-align: center;
        color: #333;
      }

      .data-row {
        margin-bottom: 8px;
      }

      .key-value-box {
      display: block;
  background-color: white;
  border: 1px solid #ccc;
  border-radius: 8px;
  padding: 16px;
  font-family: 'Helvetica Neue', sans-serif;
  font-size: 14px;
  color: #333;
  width: 100%;
  max-width: 100%;
  box-shadow: 0 2px 4px rgba(0,0,0,0.05);
}

.key-value-box .kv-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 8px;
}

hr .key-value-box {
border-top: 1px solid #333;
}

.key-value-box .key {
  font-weight: 600;
  color: #444;
  margin-right: 12px;
  max-width: 100px;
}

.key-value-box .value {
  color: #000;
  text-align: left;
  white-space: nowrap;
}

.key-value-box .kv-row:hover {
  background-color: rgba(0, 0, 0, 0.04); /* Light gray overlay */
  border-radius: 4px;
  transition: background-color 0.2s ease;
}
    ")
                 ),
      tags$script(
      HTML(
      "
      Shiny.addCustomMessageHandler('setHash', function(message) {
      window.location.hash = message;
    });
      "
           )
      )
    ),
    tags$script(inactivity),

    ## Dataframe
    #box(
    #  width = 12,
      tabsetPanel(
        id = 'tabs',
        tabPanel(
          "Events",
          value = "events_tab",
          h3('Event Logging'),

          column(
            width = 2,
            shiny::textInput("stn", label = 'Station ID', width = '10em'),
            shiny::textInput("cast", label = 'Cast Number', width = '10em'),
            shiny::textInput("transect", label = 'Transect Name', width = '10em')
          ),
          column(
            width = 4,
            shiny::selectInput("instrument",
                               label = labelMandatory('Instrument'),
                               choices = names(instruments),
                               width = '35em'),
            shiny::selectInput("author",
                               label = labelMandatory('Author/Event Owner'),
                               choices = authors,
                               width = '35em')
          ),
          column(
            width = 2,
            textInput('depthBottom', label = 'Bottom Depth (m)', width = '10em'),
            textInput('depthMaximum', label = 'Deployed Depth (m)', width = '10em')
          ),
          column(
            width = 3,
            textAreaInput("notes", label = 'Entry Notes',
                          width = '45em', height = '7em'),
          ),
          column(
            width = 2,
            actionButton('queue', label = 'Queue Event')
          ),
          column(
            width = 2,
            actionButton('clear.enter', label = 'Reset')
          ),
          column(
            width = 12,
            hr(),
            br(),
            DT::dataTableOutput("events", fill = T, height = 450)
          )
        ),
        tabPanel(
          "Details",
          value = "details_tab",
          uiOutput('entry_header'),
          hr(),
          uiOutput('override_buttons'),
          br(),
          DT::dataTableOutput("entry_info"),
          br(),
          actionButton("back_btn", "Back to Table"),
          hr(),
          column(
            width = 10,
            tags$span(
              class= 'key-value-box',
              shiny::uiOutput('entry_notes')
            )
          )
        )
      #)
    )
  )
)


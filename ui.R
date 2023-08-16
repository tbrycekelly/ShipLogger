ui = fluidPage(
  titlePanel(""),

  ## Start Layout
  sidebarLayout(

    ## Sidebar
    sidebarPanel = sidebarPanel(
      width = 3,
      h3("Cruise Event Log"),
      hr(),
      h6("Current time: "),
      h4(textOutput("currentTime", container = span)),
      h4(textOutput("currentTime.local", container = span)),
      h4(textOutput("lat", container = span)),
      h4(textOutput("lon", container = span)),
      br(),br(),
      hr(),
      fluidRow(
        h2('Export:'),
        downloadButton('download.csv', label = 'CSV'),
        downloadButton('download.xlsx', label = 'XLSX'),
        downloadButton('download.json', label = 'JSON')
      ),
      fluidRow(
        br(),
        downloadButton('download.pos', label = 'Positions')
      )
    ),

    ## Main
    mainPanel = mainPanel(
      #width = 12,

          title = "Operations",
          h2("Event Log"),

          fluidRow(
            column(
              width = 2,
              shiny::textInput("cast", label = 'Cast', width = '10em'),
              shiny::textInput("stn", label = 'Station', width = '10em')
            ),
            column(
              width = 10,
              shiny::selectInput("instrument",
                                 label = 'Instrument',
                                 choices = instruments,
                                 width = '35em'),
              textInput("entry", label = 'Entry Comments',
                        width = '35em'),
              shiny::selectInput("author",
                                 label = 'Author',
                                 choices = authors,
                                 width = '35em')
            ),
          ),
          br(),

          ## Options
          fluidRow(
            column(
              width = 9,
              radioGroupButtons("action",
                                 label = 'Actions Performed',
                                 choices = actions)
              )
            ),
          br(),

          ## Buttons
          fluidRow(
            actionButton('enter', label = 'Enter'),
            actionButton('clear.enter', label = 'Reset'),
            actionButton('refresh', label = 'Refresh', icon = icon('arrows-rotate'))
          ),

          ## Dataframe
          fluidRow(
            column(
              width = 12,
              hr(),
              br(),
              dataTableOutput("events")
            )
          )
    ) # mainPanel
  ) # sidebarLayout
)


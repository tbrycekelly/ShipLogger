library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(openxlsx)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(ulid) # For unique IDs
library(DBI)
library(RSQLite)
library(shinycssloaders)

library(SimpleMapper)# For mapping: devtools::install_github('tbrycekelly/SimpleMapper')
library(SimpleBathy) # For bathymetric data: devtools::install_github('tbrycekelly/SimpleBathy')

source('config.R')
source('functions.R')


server = function(input, output, session) {

  source('config.R')
  source('functions.R')

  globalID = NULL
  selected_id = reactiveVal(NULL)

  addLog('New session created. Loaded source files.')
  initShiplogger() # create log if doesn't exist

  Record = reactiveFileReader(intervalMillis = 1e3, session = session, filePath = settings$databaseFile,
                              readFunc = function(x) {readRecord()})

  clear = function() {
    addLog('Clearing user input fields.')
    updateSelectInput(inputId = 'author', selected = F)
    updateSelectInput(inputId = 'instrument', selected = F)
    updateTextInput(inputId = "stn", value = "")
    updateTextInput(inputId = "cast", value = "")
    updateTextInput(inputId = "depth", value = "")
    updateTextInput(inputId = 'notes', value = '')
  }

  observeEvent(
    input$exit,
    stopApp(list(input$phase_selection, input$table_selection))
  )

  #Form for data entry
  entry_form = function(button_id) {
    showModal(
      modalDialog(
        div(
          id=("entry_form"),
          tags$head(tags$style(".modal-dialog{ width:950px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            h2('Revision history:'),
            p('Double click to edit a cell. Revisions will be saved immediately.'),
            DT::dataTableOutput('history')
          ),
          easyClose = TRUE
        )
      )
    )
  }

  #Form for data entry
  note_form <- function(button_id){
    showModal(
      modalDialog(
        div(
          id=("note_form"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          fluidPage(
            h2('Add Event Note:'),
            textAreaInput('newnote', label = 'Submit', width = '350px', height = '300px'),
            actionButton(button_id, label = 'Submit')
            )
          ),
          easyClose = TRUE
        )
      )
    }

  observeEvent(
    input$clear.enter,
    {
      clear()
    }
  )

  ## Creats a new entry in the record
  observeEvent(
    input$queue,
    {
      entry = eventTemplate()
      entry$station = toupper(input$stn)
      entry$cast = input$cast
      entry$instrument = input$instrument
      entry$author = input$author
      entry$notes = input$notes
      entry$longitude = position()$lon
      entry$latitude = position()$lat
      entry$datetime = Sys.time()
      entry$event_id = ulid()
      entry$bottom_depth = NA
      #entry$bottom_depth = as.numeric(input$depthBottom)
      entry$maximum_depth = as.numeric(input$depthMaximum)

      addLog(paste0('Logging user event ', entry$group_id,'.'))
      updateRecord(entry)
      clear()
    }
  )

  observeEvent(
    input$about,
    {
      shinyalert::shinyalert(title = 'About this app',
                             text = 'This app was created by Tom Kelly (<a target = "_new_" href = "https://github.com/tbrycekelly">Github Link</a>).<br /> To learn more about this app and how to set it up please visit the <a target = "_new_" href = "http://github.com/tbrycekelly/ShipLogger">project page</a>.',
                             html = T)
    }
  )


  position = reactive({
    invalidateLater(1e3 * settings$timeouts$positionUpdate)
    response = jsonlite::read_json('http://localhost:8000/positions?limit=10') # use Fast API
    position = NULL

    while(is.null(position) & length(response) > 0) {
      if (!is.na(response[[1]]$message$latitude)) {
        position = list(datetime = isoTime(response[[1]]$timestamp), lon = response[[1]]$message$longitude, lat = response[[1]]$message$latitude)
      } else {
        response = response[-1]
      }
    }

    ## TODO: If no valid position found, handle this.
    if (abs(as.numeric(difftime(position$datetime, Sys.time(), units = 'mins'))) > 1) {
      shiny::showNotification(type = 'error', session = session, 'Last position update was >1 minute ago. Check NMEA feed if problem persists.')
    }

    ## Return
    position
  })



  # Handle clicking on a row link
  observeEvent(input$entry_click, {
    selected_id(input$entry_click)
    updateTabsetPanel(session, "tabs", selected = "details_tab")
  })


  # Go back to main table
  observeEvent(input$back_btn, {
    session$sendCustomMessage("setHash", "")
    updateTabsetPanel(session, "tabs", selected = "events_tab")
  })

  observe({
    invalidateLater(500)
    hash <- isolate(session$clientData$url_hash)

    if (nchar(hash) < 5) {
      updateTabsetPanel(session, "tabs", selected = "events_tab")
    }
  })
  observe({
    invalidateLater(500)
    hash <- isolate(session$clientData$url_hash)

    if (nchar(hash) > 5) {
      updateTabsetPanel(session, "tabs", selected = "details_tab")
    }
  })


  ## Updates the record with an updated entry based on button press
  observeEvent(
    input$button,
    {
      id = strsplit(input$button, '-')[[1]] # <group_id>-<actionNum>
      recordID = id[1]
      actionID = id[2]
      record = Record()

      if (actionID == '0') {
        k = which(recordID == record$event_id)
        if (length(k) == 1) {
          addLog(paste0('User hit note button for ', recordID))
          addLog(paste0('Updating note filed to ', record$notes[k]))
          globalID <<- recordID
          note_form("note_submit")
          updateTextAreaInput(session = session, inputId = 'newnote', value = record$notes[k])
        } else {
          stop('More than one matching event?')
        }

      } else {
        addLog(paste0('User hit button for ', recordID))

        k = which(recordID == record$group_id)
        if (length(k) < 1) { ## Sanity check
          stop('Bad. Unique ID for button slection does not match any records in log???')
        }

        ## Determine the action
        if (record$instrument[k[1]] %in% names(instruments)) {
          action = instruments[[record$instrument[k[1]]]][as.numeric(actionID)]

          newEntry = record[k[1],] ## Copy entry
          pos = position()

          ## Updated to new metadata
          newEntry$event_id = ulid()
          newEntry$action = action
          newEntry$datetime = Sys.time()
          newEntry$longitude = pos$lon
          newEntry$latitude = pos$lat
          newEntry$notes = ''
          if (buttons[[action]]$terminal) {
            newEntry$alive = F
          }

          updateRecord(newEntry)
          clear()
        } else {
          stop('No matching instrument record for this action. Bad or impossible situation?')
        }
      }
    }
  )

  observeEvent(
    input$note_submit,
    priority = 20,
    {
      addLog(paste0('Saving note.'))
      record = Record()

      #id = input$note_submit
      id = globalID
      addLog(paste0('Note for ', id))

      k = which(record$event_id == id)
      if (length(k) == 1) {
        record$notes[k] = input$newnote
        updateRecord(record[k,])
      } else {
        stop('No matching eventID. Error.')
      }
      removeModal()
    }
  )




  #### Outputs
  output$currentTime = renderText(
    {
      invalidateLater(settings$timeouts$uiRefresh * 1e3, session)
      format(Sys.time(), format = settings$datetime.format, tz = '')
    })


  output$currentTime.local = renderText(
    {
      invalidateLater(settings$timeouts$uiRefresh * 1e3, session)
      format.POSIXct(Sys.time(), format = settings$datetime.format, tz = 'GMT')
    })


  output$lon = renderUI(
    {
      pos = position()
      s = 'E'
      if (!is.na(pos$lon) & pos$lon < 0) {
        s = 'W'
        pos$lon = -pos$lon
      }
      lonMinutes = paste0(floor(pos$lon), '\u00B0', " ", round(60 * (pos$lon - floor(pos$lon)), 4), "' ", s)
      lon = paste('Lon: ', round(pos$lon, digits = 4), s)
      tagList(
        tags$span(lon),
        tags$span(lonMinutes, style = 'color:#cc5555; text-align:right;')
      )
    }
  )


  output$lat = renderUI(
    {
      pos = position()

      s = 'N'
      if (!is.na(pos$lat) & pos$lat < 0) {
        s = 'S'
        pos$lat = -pos$lat
      }
      latMinutes = paste0(floor(pos$lat), '\u00B0', " ", round(60 * (pos$lat - floor(pos$lat)), 4), "' ", s)
      lat = paste('Lat: ', round(pos$lat, digits = 4), s)
      tagList(
        tags$span(lat),
        tags$span(latMinutes, style = 'color:#cc5555; text-align:right; margin-left: 30px;')
      )
    }
  )

  output$age = renderUI(
    {
      invalidateLater(settings$timeouts$uiRefresh * 1e3)
      pos = position()
      delta = abs(as.numeric(difftime(Sys.time(), pos$datetime, units = 'secs')))

      if (delta > settings$timeouts$positionUpdate * 2) {
        return(tags$span(paste('Fix Age:', round(delta), 'seconds.'), style = 'color:red;'))
      } else {
        return(tags$span(paste('Fix Age:', round(delta), 'seconds.')))
      }
    }
  )


  output$events = renderDT(
    {
      tmp = Record()
      tmp = tmp[order(tmp$datetime, decreasing = T, na.last = T),]

      ## Add buttons
      tmp$button = addEventButtons(tmp)
      tmp$notebutton = addNoteButtons(tmp)

      ## Now make it pretty
      nice = data.frame(
        Event = substr(tmp$group_id, 22, 26),
        SystemTime = format.POSIXct(as.POSIXct(tmp$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format),
        Instrument = tmp$instrument,
        Status = tmp$action,
        Station = tmp$station,
        Cast = tmp$cast,
        Author = tmp$author,
        Action = tmp$button,
        Note = tmp$notebutton
        )


      for (i in 1:nrow(nice)) {
        nice$Event[i] = paste0('<a href="#', tmp$group_id[i], '">', nice$Event[i], '</a>')
      }
      #for (i in 1:nrow(nice)) {
      #  nice$SystemTime[i] = paste0('<a href="#', tmp$group_id[i], '">', nice$SystemTime[i], '</a>')
      #}
      nice = nice[order(tmp$datetime, decreasing = T),]
      nice = nice[!(nice$Action == '' & nice$Status == 'Queue'),]
      DT::datatable(nice,
                    editable = F,
                    filter = 'top',
                    rownames = F,
                    selection = 'single',
                    escape = F,
                    extensions = 'RowGroup',
                    options = list(
                      rowGroup = list(dataSrc = 2),
                      autoWidth = TRUE,
                      columnDefs = list(
                        list(width = '350px', targets = 7), # Buttons
                        list(width = '25px', targets = 5), # cast
                        list(width = '200px', targets = 1), # datetime
                        list(width = '100px', targets = 8) # notes
                      ),
                      pageLength = 30
                      )
                    )
    },
    server = F)


  output$history = renderDT({

    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$events_rows_selected)) {
      tmp = parseLog(getRecord())
      id = tmp$id[input$events_rows_selected]

      addLog(paste('Searching for the history of event', id, '.'))
      tmp = tmp[tmp$id == id,]

      nice = data.frame(SystemTime = format.POSIXct(as.POSIXct(tmp$time, origin = '1970-01-01 00:00:00'), format = settings$datetime.format),
                        Latitude = round(tmp$latitude, 4),
                        Longitude = round(tmp$longitude, 4),
                        Status = tmp$status,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Depth = tmp$depth,
                        Author = tmp$author)

      DT::datatable(nice, editable = T, rownames = F, escape = F, filter = 'none', autoHideNavigation = F)
    }
  })


  output$cruisemap = renderPlot(
    {
      invalidateLater(settings$timeouts$mapRefresh * 1e3)

      ## API Query
      earliestTime = isoTime(Sys.time() - 86400 * as.numeric(input$days), rev = T)
      response = jsonlite::read_json(paste0('http://localhost:8000/positions?limit=100000&by=300'))
      response = c(response, jsonlite::read_json(paste0('http://localhost:8000/positions?limit=30&by=10')))
      nmea = data.frame(datetime = NA, lon = rep(NA, length(response)), lat = NA)
      for (i in 1:length(response)) {
        nmea$datetime[i] = response[[i]]$timestamp
        nmea$lon[i] = response[[i]]$message$longitude
        nmea$lat[i] = response[[i]]$message$latitude
      }
      nmea = nmea[!is.na(nmea$lon),]
      nmea = nmea[order(nmea$datetime, decreasing = T),]

      par(plt = c(0.15,0.95,0.1,0.95))
      map = plotBasemap(lon = nmea$lon[1],
                        lat = nmea$lat[1],
                        scale = 3^(8-as.numeric(input$scale)),
                        land.col = '#222222',
                        frame = F)
      if (input$drawIsobath) {
        addIsobath(map, c(1:9)*-1e3, col = 'darkgrey', lwd = 2)
        addIsobath(map, c(1:3)*-250, col = 'darkgrey', lty = 2)
        mtext('Dashed Isobath at 250, 500, 750 m; Solid every 1,000 m', cex = 0.7, adj = 0)
      }
      addLatitude(map)
      addLongitude(map)
      addLine(map, nmea$lon, nmea$lat)
      addPoints(map, lon = nmea$lon[1], lat = nmea$lat[1], col = 'darkgreen', pch = 16, cex = 1.5)
      addScale(map)
    }
  )



  output$entry_header <- renderUI({
    record = Record()
    if (nchar(session$clientData$url_hash) > 1) {
      record = record[record$group_id == gsub('#', '', session$clientData$url_hash),]
    }

    ## Clean up values
    if (record$station[1] == '') {
      record$station[1] = "none"
    }
    if (record$cast[1] == '') {
      record$cast[1] = "none"
    }
    if (record$author[1] == '') {
      record$author[1] = "none"
    }
    if (is.na(record$maximum_depth[1])) {
      record$maximum_depth[1] = ""
    } else {
      record$maximum_depth[1] = paste0(record$maximum_depth[1])
    }
    #if (is.na(record$bottom_depth[1] == '')) { # Bottom Depth TODO from NMEA.
    #  record$bottom_depth[1] = "unknown"
    #} else {
    #  record$bottom_depth[1] = paste0('(', record$bottom_depth[1], 'm)')
    #}

    ## Build Display

    tagList(
      h3(paste0('Event ', substr(record$group_id[1], 22, 26))),
      fluidRow(
        #hr(),
        tags$span(
          column(
            width = 2,
            textInput('detail.instrument', label = 'Instrument: ', value = record$instrument[1]),
          ),
          column(
            width = 2,
            textInput('detail.station', label = 'Station: ', value = record$station[1])
          ),
          column(
            width = 2,
            textInput('detail.cast', label = 'Cast: ', value = record$cast[1])
          ),
          column(
            width = 2,
            textInput('detail.depth', label = 'Max Depth: ', value = record$maximum_depth[1])
          ),
          column(
            width = 4,
            textInput('detail.author', label = 'Author: ', value = record$author[1])
          ),
          column(
            width = 2,
            tags$span(actionButton('detail.edit', 'Update'))
          )
        )
      )
    )
  })

  observeEvent(
    input$detail.edit,
    {
      tmp = Record()
      if (nchar(session$clientData$url_hash) > 1) {
        tmp = tmp[tmp$group_id == gsub('#', '', session$clientData$url_hash),]
      } else {
        stop("Invalid edit value?")
      }

      # apply update to fields
      tmp$instrument = input$detail.instrument
      tmp$author = input$detail.author
      tmp$station = input$detail.station
      tmp$cast = input$detail.cast
      tmp$maximum_depth = input$detail.depth

      # save updated records:
      for (i in 1:nrow(tmp)) {
        updateRecord(tmp[i,])
      }
      shiny::showNotification(ui = paste0('Metadata Updated.\nTransaction recorded to database at ', format(Sys.time(), format = settings$datetime.format)),
                              duration = 5,
                              type = 'message')
    })

  output$override_buttons = renderUI({
    tmp = Record()
    if (nchar(session$clientData$url_hash) > 1) {
      tmp = tmp[tmp$group_id == gsub('#', '', session$clientData$url_hash),]
    }

    tmp$alive = T
    tmp$action = 'Queue'
    tmp$buttons = addEventButtons(tmp, TRUE)

    tagList(
      tags$span('New events: ', class = 'data-label'),
      tags$span(HTML(tmp$buttons[1]))
    )
  })


  output$entry_info = renderDT({
    record = Record()
    if (nchar(session$clientData$url_hash) > 1) {
      record = record[record$group_id == gsub('#', '', session$clientData$url_hash),]
    }
    record$notebutton = addNoteButtons(record)

    ## Now make it pretty
    nice = data.frame(Status = record$action,
                      SystemTime = format.POSIXct(as.POSIXct(record$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format),
                      SystemTimeUTC = format.POSIXct(as.POSIXct(record$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format, tz = 'UTC'),
                      Longitude = paste(round(record$longitude, 4)),
                      Latitude = paste(round(record$latitude, 4)),
                      Note = record$notebutton)

    nice = nice[order(record$datetime, decreasing = T),]
    DT::datatable(nice,
                  autoHideNavigation = T,
                  editable = T,
                  filter = 'none',
                  rownames = T,
                  selection = 'single',
                  escape = F,
                  options = list(
                    ordering = F,
                    pageLength = 10
                  ))
  }, server = F)


  observeEvent( # catch edits to entries
    input$entry_info_cell_edit,
    {
      record = Record()
      record = record[record$group_id == gsub('#', '', session$clientData$url_hash),]
      record = record[order(record$datetime, decreasing = T, na.last = T),]

      row = input$entry_info_cell_edit$row
      col = input$entry_info_cell_edit$col
      message(row, ' ', col)

      entry = record[row,]

      if (col == 1) {
        entry$action = input$entry_info_cell_edit$value
      } else if (col == 2) {
        entry$datetime = isoTime(as.POSIXct(input$entry_info_cell_edit$value, tz = '', origin = '1970-01-01 00:00:00'))
      } else if (col == 3) {
        entry$datetime = isoTime(as.POSIXct(input$entry_info_cell_edit$value, tz = 'UTC', origin = '1970-01-01 00:00:00'))
      } else if (col == 4) {
        entry$longitude = as.numeric(input$entry_info_cell_edit$value)
      } else if (col == 5) {
        entry$latitude = as.numeric(input$entry_info_cell_edit$value)
      }

      updateRecord(entry)
    })


  output$entry_notes = renderUI({
    record = Record()
    if (nchar(session$clientData$url_hash) > 1) {
      record = record[record$group_id == gsub('#', '', session$clientData$url_hash),]
    }

    notes = paste0(tags$div(tags$h4('Notes'), class = "kv-row"))
    for (i in 1:nrow(record)) {
        if (nchar(record$note[i]) > 0) {
          notes = paste0(notes,
                         tags$div(
                           tags$div(record$action[i], class = "key"), tags$div(record$note[i], class = "value"),
                           class = "kv-row"
                           )
                         )
      }
    }

    HTML(notes)
  })


  #### Download options:
  output$download = downloadHandler(
    filename = function() {
        paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.sqlite')
      },
    content = function (file) {
      addLog('Preparing SQLite download.')
      file.copy(settings$databaseFile, to = file)
    })

  output$downloadXLSX = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      addLog('Preparing xlsx download.')
      tmp = readRecord()
      tmp$datetime = format(as.POSIXct(tmp$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format)
      tmp$datetimeUTC = format(as.POSIXct(tmp$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format, tz = 'UTC')

      openxlsx::write.xlsx(tmp, file)
    })


  output$downloadJSON = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      addLog('Preparing JSON download.')
      tmp = readRecord()
      tmp$datetime = format(as.POSIXct(tmp$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format)
      tmp$datetimeUTC = format(as.POSIXct(tmp$datetime, origin = '1970-01-01 00:00:00'), format = settings$datetime.format, tz = 'UTC')

      jsonlite::write_json(tmp, file, pretty = T)
    })


  output$downloadPositions = downloadHandler(
    filename = function() {
      paste0('ShipLogger Position Record ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      addLog('Preparing Positions download.')
      response = jsonlite::read_json('http://localhost:8000/positions?by=60')
      write_json(response, path = file)
    })
}

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


server = function(input, output, session) {

  source('config.R')
  source('functions.R')

  globalID = NULL

  addLog('New session created. Loaded source files.')
  QueuedRecord() # create log if doesn't exist

  getRecord = reactiveFileReader(
    intervalMillis = settings$timeouts$logRefresh * 1e3,
    filePath = 'log/log.rds',
    readFunc = readRDS,
    session = session
    )

  currentRecord = reactive(
    parseLog(getRecord())
  )

  addLog('Queuedialization finished.')

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

  ## Updates the record with an updated entry based on button press
  observeEvent(
    input$button,
    {
      id = strsplit(input$button, '-')[[1]]
      recordID = id[1]
      actionID = id[2]
      record = getRecord()

      if (actionID == '0') {
        addLog(paste0('User hit note button for ', recordID))
        addLog(paste0('Updating note filed to ', record[[recordID]]$notes))
        globalID <<- recordID
        note_form("note_submit")
        updateTextAreaInput(session = session, inputId = 'newnote', value = record[[recordID]]$notes)

      } else {
        addLog(paste0('User hit button for ', recordID))

        k = which(recordID == names(record))
        if (length(k) != 1) { ## Sanity check
          print('Bad')
          stop()
        }

        ## Determine the action
        if (record[[recordID]]$instrument %in% names(instruments)) {
          currentActions = instruments[[record[[recordID]]$instrument]]
          action = currentActions[as.numeric(actionID)]

          updatedEntry = record[[recordID]] ## Copy entry
          pos = position()
          updatedEntry$events[[action]] = list(status = action,
                                                time = Sys.time(),
                                                longitude = pos$lon,
                                                latitude = pos$lat)
          appendRecord(updatedEntry)
          clear()
        }
      }
    }
  )

  ## Creats a new entry in the record
  observeEvent(
    input$queue,
    {
      entry = blank.event()[[1]]
      addLog(paste('Logging user event', entry$id,'.'))
      entry$station = toupper(input$stn)
      entry$cast = input$cast
      entry$instrument = input$instrument
      entry$author = input$author
      entry$notes = input$notes
      entry$depth = input$depth
      entry$events$Queued$longitude = position()$lon
      entry$events$Queued$latitude = position()$lat
      entry$events$Queued$time = Sys.time()

      appendRecord(entry)
      clear()
    }
  )


  observeEvent(
    input$note_submit,
    priority = 20,
    {
      addLog(paste0('Saving note.'))
      record = getRecord()

      id = input$note_submit
      id = globalID
      addLog(paste0('Note for ', id))

      updatedRecord = record[[id]]
      updatedRecord$notes = input$newnote
      appendRecord(updatedRecord)
      removeModal()
    }
  )


  observeEvent(
    input$edit_button,
    {
      tmp = parseLog(getRecord())

      row  = input$events_rows_selected
      addLog(paste('Entry selected for review is ', row, '.'))
      if (length(row) > 0) {
        entry_form("submit_edit")
      }
    }
  )


  observeEvent(
    input$delete_button,
    {
      row  = input$events_rows_selected
      if (length(row) == 1) {
        displayed = currentRecord()
        id = displayed$id[row]
        updatedRecord = getRecord()[[id]]
        updatedRecord$events[['DELETED']] = list( status = 'DELETED',
                                                  time = Sys.time(),
                                                  longitude = NA,
                                                  latitude = NA)

        appendRecord(updatedRecord)
        clear()
      }
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

  observeEvent(
    input$refresh,
    {
      shiny::showNotification(type = 'message', session = session, 'Logger Running.')
      clear()
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
    if (abs(as.numeric(difftime(position$datetime, Sys.time(), units = 'mins'))) > 1) {
      shiny::showNotification(type = 'error', session = session, 'Last position update was >1 minute ago. Check NMEA feed if problem persists.')
    }

    ## Return
  position
  })


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


  output$lon = renderText(
    {
      pos = position()
      s = 'E'
      if (!is.na(pos$lon) & pos$lon < 0) {
        s = 'W'
        pos$lon = -pos$lon
      }
      paste('Lon:', round(pos$lon, digits = 4), s)
    }
  )


  output$lat = renderText(
    {
      pos = position()
      s = 'N'
      if (!is.na(pos$lat) & pos$lat < 0) {
        s = 'S'
        pos$lat = -pos$lat
      }
      paste('Lat:', round(pos$lat, digits = 4), s)
    }
  )

  output$age = renderText(
    {
      invalidateLater(settings$timeouts$uiRefresh * 1e3)
      pos = position()
      delta = abs(as.numeric(difftime(Sys.time(), pos$datetime, units = 'secs')))
      paste('Fix Age:', round(delta), 'seconds.')
    }
  )


  output$events = renderDT(
    {
      tmp = currentRecord()
      tmp = tmp[order(tmp$time, decreasing = T, na.last = T),]

      ## Add buttons
      tmp$button = ''
      for (i in 1:nrow(tmp)) {
        if (tmp$instrument[i] %in% names(instruments)) {
          if (i == min(which(tmp$id == tmp$id[i]))) { # Make button only for the latest entry for an ID.
            currentActions = unique(tmp$status[tmp$id == tmp$id[i]]) ## List of actions that have been performed: e.g. 'Queued', 'Deploy'
            possibleActions = which(!instruments[[tmp$instrument[i]]] %in% currentActions) ## list of actions left to be performed: e.g. 'Deploy', 'Recover'

            if (length(possibleActions) > 0) {
              nextAction = instruments[[tmp$instrument[i]]][min(possibleActions)]
              tmp$button[i] = shinyInput(FUN = actionButton,
                                         id = paste0(tmp$id[i], '-', min(possibleActions)),
                                         label = nextAction,
                                         onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})',
                                         style=paste0("color: ", color[[nextAction]]$text, "; background-color: ", color[[nextAction]]$bkg, "; border-color: #2e6da4")
              )
            }
          } else {
            #tmp$station[i] = NA
            #tmp$cast[i] = NA
            #tmp$author[i] = NA
          }
        }
      }

      ## Add button for notes
      tmp$notebutton = ''
      for (i in 1:nrow(tmp)) {
        if (nchar(tmp$notes[i]) > 0) {
        tmp$notebutton[i] = paste0(shinyInput(FUN = actionButton,
                                   id = paste0(tmp$id[i], '-0'),
                                   label = 'See Notes',
                                   onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})',
                                   style="color: #fff; background-color: #444; border-color: #2e6da4"
            ), ' &#9733;')
        } else {
          tmp$notebutton[i] = shinyInput(FUN = actionButton,
                                                id = paste0(tmp$id[i], '-0'),
                                                label = 'See Notes',
                                                onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})',
                                                style="color: #fff; background-color: #444; border-color: #2e6da4"
          )
        }
      }

      nice = data.frame(SystemTime = format.POSIXct(as.POSIXct(tmp$time), format = settings$datetime.format),
                        Location = paste('Lat:', round(tmp$latitude, 4), '<br>Lon:', round(tmp$longitude, 4)),
                        Action = tmp$button,
                        Status = tmp$status,
                        Instrument = tmp$instrument,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Depth = tmp$depth,
                        Author = tmp$author,
                        Note = tmp$notebutton)
      k = !(nice$Status == 'Queued' & nice$Action == '' & nice$Instrument != '')
      nice = nice[k,]
      nice = nice[order(tmp$time, decreasing = T),]
      DT::datatable(nice,
                    editable = F,
                    filter = 'top',
                    rownames = F,
                    selection = 'single',
                    escape = F,
                    extensions = 'RowGroup',
                    options = list(rowGroup = list(dataSrc = 4)))
    })


  observeEvent(
    input$history_cell_edit,
    {
      record = getRecord()
      tmp = parseLog(record)

      row = input$history_cell_edit$row
      id = tmp$id[input$events_rows_selected]
      colname = c('time', 'latitude', 'longitude', 'status', 'station', 'cast', 'depth', 'author')[input$history_cell_edit$col + 1]
      status = tmp$status[row]
      updatedRecord = record[[id]]
      addLog(paste0('Updating entry ', row, ' of ', colname, ' for entry ', id, '.'))
      addLog(paste0('New value is ', input$history_cell_edit$value))

      if (colname == 'time') {
        updatedRecord$events[[status]][[colname]] = as.POSIXct(input$history_cell_edit$value, format = settings$datetime.format)
      } else if (colname %in% c('time', 'latitude', 'longitude')) {
        updatedRecord$events[[status]][[colname]] = input$history_cell_edit$value
      } else if (colname %in% c('station', 'cast', 'depth', 'author')) {
        updatedRecord[[colname]] = input$history_cell_edit$value
      } else if (colname == 'status') {
        updatedRecord$events[[status]]$status = input$history_cell_edit$value
        names(updatedRecord$events)[which(names(updatedRecord$events) == status)] = input$history_cell_edit$value
      } else {
        # Warn?
      }

      appendRecord(updatedRecord)
      removeModal()
  })

  output$history = renderDT({

    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$events_rows_selected)) {
      tmp = parseLog(getRecord())
      id = tmp$id[input$events_rows_selected]

      addLog(paste('Searching for the history of event', id, '.'))
      tmp = tmp[tmp$id == id,]

      nice = data.frame(SystemTime = format.POSIXct(as.POSIXct(tmp$time), format = settings$datetime.format),
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
      response = jsonlite::read_json(paste0('http://localhost:8000/positions?limit=100000&by=120&since=', earliestTime))
      response = c(response, jsonlite::read_json(paste0('http://localhost:8000/positions?limit=360&by=10&since=', earliestTime)))
      nmea = data.frame(datetime = NA, lon = rep(NA, length(response)), lat = NA)
      for (i in 1:length(response)) {
        nmea$datetime[i] = response[[i]]$timestamp
        nmea$lon[i] = response[[i]]$message$longitude
        nmea$lat[i] = response[[i]]$message$latitude
      }
      nmea = nmea[!is.na(nmea$lon),]
      nmea = nmea[order(nmea$datetime, decreasing = T),]

      par(plt = c(0.12,1,0.1,1))
      map = plotBasemap(lon = nmea$lon[1],
                        lat = nmea$lat[1],
                        scale = 3^as.numeric(input$scale),
                        land.col = 'black',
                        frame = F)
      map = addLatitude(map)
      map = addLongitude(map)
      map = addLine(map, nmea$lon, nmea$lat)
      map = addPoints(map, lon = nmea$lon[1], lat = nmea$lat[1])
      map = addScale(map)
    }
  )

  #### Download options:
  output$download.csv = downloadHandler(
    filename = function() {
        paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.csv')
      },
    content = function (file) {
      addLog('Preparing csv download.')
      tmp = parseLog(getRecord())
      tmp$time = format(as.POSIXct(tmp$time), format = settings$datetime.format)
      write.csv(tmp, file)
    })


  output$download.xlsx = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      addLog('Preparing xlsx download.')
      tmp = parseLog(getRecord())
      tmp$time = format(as.POSIXct(tmp$time), format = settings$datetime.format)
      openxlsx::write.xlsx(tmp, file)
    })


  output$download.json = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      addLog('Preparing JSON download.')
      tmp = readRDS('log/log.rds')
      jsonlite::write_json(tmp, file, pretty = T)
    })


  output$download.pos = downloadHandler(
    filename = function() {
      paste0('ShipLogger Position Record ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      addLog('Preparing Positions download.')
      response = jsonlite::read_json('http://localhost:8000/positions?by=60')
      write(response, file)
    })
}

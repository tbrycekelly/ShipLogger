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


server = function(input, output, session) {


  source('config.R')
  source('functions.R')

  add.log('New session created. Loaded source files.')
  init.log() # create log if doesn't exist

  log = reactiveFileReader(
    intervalMillis = 1e3,
    filePath = 'log/log.json',
    readFunc = load.log,
    session = session
    )

  parseLog = reactive(
    {
      tmp = parse.log(log())
      tmp[tmp$status != 'DELETED',]
    }
  )

  add.log('Initialization finished.')

  clear = function() {
    tmp = parseLog()
    add.log('Clearing user input fields.')
    updateTextInput(inputId = "event", value = max(as.numeric(tmp$event), na.rm = T) + 1)
    updateSelectInput(inputId = 'author', selected = F)
    updateSelectInput(inputId = 'instrument', selected = F)
    updateTextInput(inputId = "stn", value = "")
    updateTextInput(inputId = "cast", value = "")
    updateTextInput(inputId = 'entry', value = '')
  }

  observeEvent(
    input$exit,
    stopApp(list(input$phase_selection, input$table_selection))
  )


  #Form for data entry
  entry_form <- function(button_id){
    Sys.sleep(0.2)
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:950px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("200px", "100px"),
                  selectInput('event.instrument', 'Instrument', instruments)
                ),
                splitLayout(
                  cellWidths = c("200px", "100px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("event.time.start", "Start Time", value = ''),
                  textInput("event.lon.start", "Lon", placeholder = ""),
                  textInput("event.lat.start", "Lat", placeholder = "")
                ),
                splitLayout(
                  cellWidths = c("200px", "100px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("event.time.end", "End Time", value = ''),
                  textInput("event.lon.end", "Lon", placeholder = ""),
                  textInput("event.lat.end", "Lat", placeholder = "")
                ),
                splitLayout(
                  cellWidths = c("175px", "112px", "112px"),
                  cellArgs = list(style = "vertical-align: top"),
                  selectInput('event.author', 'Author', authors),
                  textInput("event.station", 'Stn', placeholder = ""),
                  textInput("event.cast", 'Cast', placeholder = "")
                ),

                textAreaInput("event.note", "Note", placeholder = "", height = 100, width = "354px"),
                textInput("note.author", "Edit Author", placeholder = ""),
                actionButton(button_id, "Submit"),
                shiny::hr(),
                shiny::hr(),
                p('Revision history:'),
                DT::dataTableOutput('history')
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }

  observeEvent(
    input$clear.enter,
    {
      clear()
    }
  )

  observeEvent(
    input$start_button,
    {
      id = input$start_button
      add.log(paste0('Start button for ', id))
      raw = log()
      parse = parseLog()

      i = 1
      while (i <= length(raw)) {
        if (!is.na(raw[[i]]$event.id) & raw[[i]]$event.id == id) {
          raw = raw[[i]]
          raw$start.time = Sys.time()
          raw$start.lon = position()$lon
          raw$start.lat = position()$lat
          raw$status = 'STARTED'
          add.log('Writting update to log file.')
          write.json('log/log.json', raw)
          clear()
          return()
        }
        i = i + 1
      }
    }
  )

  observeEvent(
    input$atdepth_button,
    {
      id = input$atdepth_button
      add.log(paste0('ATDEPTH button for ', id))
      raw = log()
      parse = parseLog()

      i = 1
      while (i <= length(raw)) {
        if (!is.na(raw[[i]]$event.id) & raw[[i]]$event.id == id) {
          raw = raw[[i]]
          raw$atdepth.time = Sys.time()
          raw$atdepth.lon = position()$lon
          raw$atdepth.lat = position()$lat
          raw$status = 'ATDEPTH'
          add.log('Writting update to log file.')
          write.json('log/log.json', raw)
          clear()
          return()
        }
        i = i + 1
      }
    }
  )

  observeEvent(
    input$end_button,
    {
      id = input$end_button
      message('End button for ', id)
      raw = log()
      parse = parseLog()

      i = 1
      while (i <= length(raw)) {
        if (!is.na(raw[[i]]$event.id) & raw[[i]]$event.id == id) {
          raw = raw[[i]]
          raw$end.time = Sys.time()
          raw$end.lon = position()$lon
          raw$end.lat = position()$lat
          raw$status = 'ENDED'
          write.json('log/log.json', raw)
          clear()
          return()
        }
        i = i + 1
      }
    }
  )


  observeEvent(
    input$queue,
    {
      entry = blank.event()
      add.log(paste('Logging user event', entry$event.id,'.'))
      entry$event = length(unique(parseLog()$event.id))+1
      entry$station = toupper(input$stn)
      entry$cast = input$cast
      entry$instrument = input$instrument
      entry$author = input$author
      entry$notes = input$notes

      write.json(filename = 'log/log.json', entry = entry)
      Sys.sleep(1)
      clear()
    }
  )


  observeEvent(
    input$submit_edit,
    priority = 20,
    {
      message(Sys.time(), ': Submit Edit.')
      raw = log()
      parse = parseLog()
      row  = input$events_rows_selected
      id = parse$event.id[row]
      message('Searching for ', id)
      i = 1
      while(i <= length(raw)) {
        if (raw[[i]]$event.id == id) {
          entry = raw[[i]]

          entry = update.conpare(entry, 'start.time', 'event.time.start')
          entry = update.conpare(entry, 'start.lon', 'event.lon.start')
          entry = update.conpare(entry, 'start.lat', 'event.lat.start')
          entry = update.conpare(entry, 'end.time', 'event.time.end')
          entry = update.conpare(entry, 'end.lon', 'event.lon.end')
          entry = update.conpare(entry, 'end.lat', 'event.lat.end')
          entry = update.conpare(entry, 'author', 'event.author')
          entry = update.conpare(entry, 'station', 'event.station')
          entry = update.conpare(entry, 'cast', 'event.cast')
          entry = update.conpare(entry, 'instrument', 'event.instrument')
          entry = update.conpare(entry, 'notes', 'event.note')
          entry$notes = paste0(entry$notes, ' (',input$note.author, ')')
          break
        }
        i = i + 1
      }
      write.json('log/log.json', entry)
      removeModal()
    }
  )

  update.conpare = function(entry, name, input.name) {
    message('Comparing ', name)
    if (is.na(entry[[name]]) | (entry[[name]] != input[[input.name]])) {
      add.log(message = paste('Changed', name, 'in', entry$event.id, 'from', entry[[name]], 'to', input[[input.name]]))
      entry[[name]] = input[[input.name]]
    }
    entry
  }

  observeEvent(
    input$edit_button,
    {
      tmp = parseLog()

      row  = input$events_rows_selected
      add.log(paste('Entry selected for review is ', row, '.'))
      if (length(row) > 0) {
        entry_form("submit_edit")
        updateTextInput(session, "event.time.start", value = tmp$start.time[input$events_rows_selected])
        updateTextInput(session, "event.lon.start", value = tmp$start.lon[input$events_rows_selected])
        updateTextInput(session, "event.lat.start", value = tmp$start.lat[input$events_rows_selected])
        updateTextInput(session, "event.time.end", value = tmp$end.time[input$events_rows_selected])
        updateTextInput(session, "event.lon.end", value = tmp$end.lon[input$events_rows_selected])
        updateTextInput(session, "event.lat.end", value = tmp$end.lat[input$events_rows_selected])
        updateSelectInput(session, "event.author", selected = tmp$author[input$events_rows_selected])
        updateTextInput(session, "event.station", value = tmp$station[input$events_rows_selected])
        updateTextInput(session, "event.cast", value = tmp$cast[input$events_rows_selected])
        updateSelectInput(session, "event.instrument", selected = tmp$instrument[input$events_rows_selected])
        updateTextInput(session, "event.event", value = tmp$event[input$events_rows_selected])

        updateTextAreaInput(session, "event.note", value = tmp$notes[input$events_rows_selected])
      }
    }
  )

  observeEvent(
    input$delete_button,
    {
      row  = input$events_rows_selected
      if (length(row) == 1) {
        raw = log()
        parse = parseLog()
        add.log(paste0('Entry selected for deletion is ', row, ' (', parse$event.id[row],').'))
        for (i in 1:length(raw)) {
          if (parse$event.id[row] == raw[[i]]$event.id) {
            raw = raw[[i]]
            raw$status = 'DELETED'
            write.json('log/log.json', raw)
            clear()
            return()
          }
        }
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


  position = reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = 'log/last.rds',
    readFunc = function(x) {
      latest = readRDS(x)
      if (abs(as.numeric(difftime(latest$time, Sys.time(), units = 'mins'))) > 1) {
        shiny::showNotification(type = 'error', session = session, 'Last position update was >1 minute ago. Check NMEA feed if problem persists.')
      }
      tmp = strsplit(latest$sentance, split = ',')

      time.raw = tmp[[1]][2]
      lat.raw = tmp[[1]][3]
      lon.raw = tmp[[1]][5]
      north = toupper(tmp[[1]][4]) == 'N'
      east = toupper(tmp[[1]][6]) == 'E'

      lat = strsplit(lat.raw, '\\.')[[1]] ## e.g. 5057.4567
      lon = strsplit(lon.raw, '\\.')[[1]] # e.g. 13745.5678

      ##TODO
      lat = as.numeric(substr(lat[1], 1, nchar(lat[1]) - 2)) + as.numeric(substr(lat[1], nchar(lat[1])-1, nchar(lat[1])))/60 + as.numeric(paste0('0.', lat[2]))/60
      lon = as.numeric(substr(lon[1], 1, nchar(lon[1]) - 2)) + as.numeric(substr(lon[1], nchar(lon[1])-1, nchar(lon[1])))/60 + as.numeric(paste0('0.', lon[2]))/60

      if (!north) {
        lat = -lat
      }
      if (!east) {
        lon = -lon
      }

      list(lon = lon, lat = lat, time = latest$time)
    }
  )


  #### Outputs
  output$currentTime = renderText(
    {
      invalidateLater(1000, session)
      paste(format(Sys.time()), ' (system)')
    })


  output$currentTime.local = renderText(
    {
      invalidateLater(1000, session)
      paste(format(Sys.time() + settings$local.timezone*3600), ' (local)')
    })


  output$lon = renderText(
    {
      pos = position()
      s = 'E'
      if (!is.na(pos$lon) & pos$lon < 0) {
        s = 'W'; pos$lon = -pos$lon
      }
      paste('Lon:', round(pos$lon, digits = 4), s)
    }
  )


  output$lat = renderText(
    {
      pos = position()
      s = 'N'
      if (!is.na(pos$lat) & pos$lat < 0) {
        s = 'S'; pos$lat = -pos$lat
      }
      paste('Lat:', round(pos$lat, digits = 4), s)
    }
  )

  output$age = renderText(
    {
      invalidateLater(1000)
      pos = position()
      delta = abs(as.numeric(difftime(Sys.time(), pos$time, units = 'secs')))
      paste('Fix Age:', round(delta), 'seconds.')
    }
  )


  output$events = renderDT(
    {
      tmp = parseLog()

      tmp$button = ''
      for (i in 1:nrow(tmp)) {
        if (tmp$status[i] == 'QUEUED') { ## Add start button
          add.log(paste0('Adding queue button for event ', tmp$event.id[i]))
          tmp$button[i] = shinyInput(FUN = actionButton,
                                     id = tmp$event.id[i],
                                     label = "Start",
                                     onclick = 'Shiny.setInputValue(\"start_button\", this.id, {priority: \"event\"})',
                                     style="color: #fff; background-color: #33aa77; border-color: #2e6da4"
                                     )
        } else if (tmp$status[i] == 'ATDEPTH') { ## Add STOP button
          tmp$button[i] = shinyInput(FUN = actionButton,
                                     id = tmp$event.id[i],
                                     label = "End",
                                     onclick = 'Shiny.setInputValue(\"end_button\", this.id, {priority: \"event\"})',
                                     style="color: #fff; background-color: #d37a57; border-color: #ee0000"
                                     )
        } else if (tmp$status[i] == 'STARTED') { ## Add ATDEPTH button
          tmp$button[i] = shinyInput(FUN = actionButton,
                                     id = tmp$event.id[i],
                                     label = "ATDEPTH",
                                     onclick = 'Shiny.setInputValue(\"atdepth_button\", this.id, {priority: \"event\"})',
                                     style="color: #fff; background-color: #435aa7; border-color: #ee0000"
          )
        }
      }

      nice = data.frame(Action = tmp$button,
                        Status = tmp$status,
                        #Event.ID = tmp$event.id,
                        Event = tmp$event,
                        Instrument = tmp$instrument,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Start = '',
                        AtDepth = '',
                        End = '',
                        Author = tmp$author,
                        Notes = tmp$notes)
      k = tmp$start.time != ''
      nice$Start[k] = paste(tmp$start.time[k], '<br>Lon:', tmp$start.lon[k], '<br>Lat:', tmp$start.lat[k])

      k = tmp$atdepth.time != ''
      nice$AtDepth[k] = paste(tmp$atdepth.time[k], '<br>Lon:', tmp$atdepth.lon[k], '<br>Lat:', tmp$atdepth.lat[k])

      k = tmp$end.time != ''
      nice$End[k] = paste(tmp$end.time[k], '<br>Lon:', tmp$end.lon[k], '<br>Lat:', tmp$end.lat[k])

      DT::datatable(nice, editable = F, filter = 'top', rownames = F, selection = 'single', escape = F)
    })


  output$history = renderDT({

    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$events_rows_selected)) {
      tmp = parse.log(log(), T)
      tab = parseLog()

      add.log(paste('Searching for the history of event', tab$event.id[input$events_rows_selected], '.'))
      tmp = tmp[tab$event.id[input$events_rows_selected] == tmp$event.id,]

      nice = data.frame(Status = tmp$status,
                        Event.ID = tmp$event.id,
                        Event = tmp$event,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Start = '',
                        End = '')
      k = tmp$start.time != ''
      nice$Start[k] = paste(tmp$start.time[k], '<br>Lon:', tmp$start.lon[k], '<br>Lat:', tmp$start.lat[k])
      k = tmp$end.time != ''
      nice$End[k] = paste(tmp$end.time[k], '<br>Lon:', tmp$end.lon[k], '<br>Lat:', tmp$end.lat[k])

      DT::datatable(nice, editable = F, rownames = F, escape = F, filter = 'none', autoHideNavigation = F)
    }
  })


  #### Download options:
  output$download.csv = downloadHandler(
    filename = function() {
        paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.csv')
      },
    content = function (file) {
      add.log('Preparing csv download.')
      tmp = parseLog()
      nice = data.frame(Status = tmp$status,
                        Event.ID = tmp$event.id,
                        Event = tmp$event,
                        Instrument = tmp$instrument,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Start.Time = tmp$start.time,
                        Start.Lon = tmp$start.lon,
                        Start.Lat = tmp$start.lat,
                        End.Time = tmp$end.time,
                        End.Lon = tmp$end.lon,
                        End.Lat = tmp$end.lat,
                        Author = tmp$author,
                        Notes = tmp$notes)
      if (!settings$demo.mode) {
        write.csv(nice, file)
      }
    })


  output$download.xlsx = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      add.log('Preparing xlsx download.')
      tmp = parseLog()

      nice = data.frame(Status = tmp$status,
                        Event.ID = tmp$event.id,
                        Event = tmp$event,
                        Instrument = tmp$instrument,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Start.Time = tmp$start.time,
                        Start.Lon = tmp$start.lon,
                        Start.Lat = tmp$start.lat,
                        End.Time = tmp$end.time,
                        End.Lon = tmp$end.lon,
                        End.Lat = tmp$end.lat,
                        Author = tmp$author,
                        Notes = tmp$notes)
      if (!settings$demo.mode) {
        openxlsx::write.xlsx(nice, file)
      }
    })


  output$download.json = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      add.log('Preparing JSON download.')
      tmp = readLines('log/log.json')
      if (!settings$demo.mode) {
        writeLines(tmp, file)
      }
    })


  output$download.pos = downloadHandler(
    filename = function() {
      paste0('ShipLogger Position Record ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      add.log('Preparing Positions download.')
      tmp = read.csv('log/nmea.csv', header = F, sep = ';')
      colnames(tmp) = c('SystemTime', 'Sentence')
      if (!settings$demo.mode) {
        openxlsx::write.xlsx(tmp, file)
      }
    })
}

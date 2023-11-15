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
    id = idList()
    add.log('Clearing user input fields.')
    updateTextInput(inputId = "start.event", value = max(as.numeric(tmp$event)) + 1)
    updateTextInput(inputId = "start.sample", value = max(id$used) + 1)
    updateSelectInput(inputId = 'author', selected = F)
    updateSelectInput(inputId = 'instrument', selected = F)
    updateTextInput(inputId = "stn", value = "")
    updateTextInput(inputId = "cast", value = "")
    updateTextInput(inputId = "n", value = "1")
    updateTextInput(inputId = 'entry', value = '')
  }


  #Form for data entry
  entry_form <- function(button_id){
    Sys.sleep(0.2)
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:860px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(
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
                selectInput('event.instrument', 'Instrument', instruments),
                textAreaInput("event.ids", "Assigned IDs", placeholder = "", height = 75, width = "354px"),
                textAreaInput("event.note", "Note", placeholder = "", height = 100, width = "354px"),
                textInput("note.author", "Edit Author", placeholder = ""),
                actionButton(button_id, "Submit"),

                shiny::hr(),
                p('Revision history:'),
                htmlOutput('history')
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
      id <- as.numeric(input$start_button)
      message('Start button for ', id)
      raw = log()
      parse = parseLog()

      i = 1
      while (i <= length(raw)) {
        if (!is.na(raw[[i]]$id) & raw[[i]]$id == id) {
          raw = raw[[i]]
          raw$start.time = Sys.time()
          raw$start.lon = position()$lon
          raw$start.lat = position()$lat
          raw$status = 'STARTED'
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
      id <- as.numeric(input$end_button)
      message('End button for ', id)
      raw = log()
      parse = parseLog()

      i = 1
      while (i <= length(raw)) {
        if (!is.na(raw[[i]]$id) & raw[[i]]$id == id) {
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


  idList = reactive({
    parse = parseLog()
    used = paste(parse$event, collapse = ',', sep = ',')
    used = as.numeric(strsplit(used, split = ',')[[1]])
    used = used[!is.na(used)]

    updateTextAreaInput(session, inputId = 'start.event', value = max(as.numeric(parse$id), na.rm = T) + 1)
    updateTextAreaInput(session, inputId = 'start.sample', value = max(used, na.rm = T) + 1)

    list(all = settings$sample.ids, used = used, avail = settings$sample.ids[!settings$sample.ids %in% used])
  })


  observeEvent(
    input$queue,
    {
      entry = blank.event()
      entry$id = as.numeric(input$start.event)
      add.log(paste('Logging user event', input$start.event,'.'))
      if (is.na(as.numeric(input$start.event))) {
        shiny::showNotification(type = 'error', 'Geotraces Sample ID not recognized as valid (must be numeric). No event queued.')
        return()
      }

      entry$station = toupper(input$stn)
      entry$cast = input$cast
      entry$instrument = input$instrument
      entry$author = input$author
      entry$notes = input$entry
      entry$n = as.numeric(input$n)
      if (!is.na(entry$n) & entry$n == 0) {
        entry$events = NA
      } else{
        if (is.na(as.numeric(input$start.sample))) {
          entry$events = max(idList()$used, na.rm = T) + 1
          shiny::showNotification(type = 'warning', 'Events initial Geotraces ID not given (or recognized). Automatically identifying next available.')
        } else {
          entry$events = as.numeric(input$start.sample)
        }
        entry$events = paste(entry$events:(entry$events + as.numeric(input$n) - 1), collapse = ', ')
      }

      write.json(filename = 'log/log.json', entry = entry)
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
      id = parse$id[row]
      message('Searching for ', id)
      i = 1
      while(i <= length(raw)) {
        if (raw[[i]]$id == id) {
          entry = raw[[i]]

          entry = update.conpare(entry, 'start.time', 'event.time.start')
          entry = update.conpare(entry, 'start.lon', 'event.lon.start')
          entry = update.conpare(entry, 'start.lat', 'event.lat.start')
          entry = update.conpare(entry, 'end.time', 'event.time.end')
          entry = update.conpare(entry, 'end.lon', 'event.lon.end')
          entry = update.conpare(entry, 'end.lat', 'event.lat.end')
          entry = update.conpare(entry, 'events', 'event.ids')
          entry = update.conpare(entry, 'author', 'event.author')
          entry = update.conpare(entry, 'station', 'event.station')
          entry = update.conpare(entry, 'cast', 'event.cast')
          entry = update.conpare(entry, 'instrument', 'event.instrument')
          entry = update.conpare(entry, 'events', 'event.ids')
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
    if (entry[[name]] != input[[input.name]]) {
      add.log(message = paste('Changed', name, 'in', entry$id, 'from', entry[[name]], 'to', input[[input.name]]))
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

        updateTextAreaInput(session, "event.ids", value = tmp$events[input$events_rows_selected])

        updateSelectInput(session, "event.author", selected = tmp$author[input$events_rows_selected])
        updateTextInput(session, "event.station", value = tmp$station[input$events_rows_selected])
        updateTextInput(session, "event.cast", value = tmp$cast[input$events_rows_selected])
        updateTextInput(session, 'event.n', value = tmp$n[input$events_rows_selected])
        updateSelectInput(session, "event.instrument", selected = tmp$instrument[input$events_rows_selected])

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
        add.log(paste0('Entry selected for deletion is ', row, ' (', parse$id[row],').'))
        for (i in 1:length(raw)) {
          if (parse$id[row] == raw[[i]]$id) {
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
        if (tmp$status[i] == 'QUEUED') {
          tmp$button[i] = shinyInput(FUN = actionButton,
                                     id = paste0(tmp$id[i]),
                                     label = "Start",
                                     onclick = 'Shiny.setInputValue(\"start_button\", this.id, {priority: \"event\"})',
                                     style="color: #fff; background-color: #33aa77; border-color: #2e6da4"
                                     )
        } else if (tmp$status[i] == 'STARTED') {
          tmp$button[i] = shinyInput(FUN = actionButton,
                                     id = paste0(tmp$id[i]),
                                     label = "End",
                                     onclick = 'Shiny.setInputValue(\"end_button\", this.id, {priority: \"event\"})',
                                     style="color: #fff; background-color: #d37a57; border-color: #ee0000"
                                     )
        }
      }

      nice = data.frame(Action = tmp$button,
                        Status = tmp$status,
                        Event.ID = tmp$id,
                        Sample.IDs = tmp$events,
                        Instrument = tmp$instrument,
                        Station = tmp$station,
                        Cast = tmp$cast,
                        Start = '',
                        End = '',
                        Author = tmp$author,
                        Notes = tmp$notes)
      k = tmp$start.time != ''
      nice$Start[k] = paste(tmp$start.time[k], '<br>Lon:', tmp$start.lon[k], '<br>Lat:', tmp$start.lat[k])
      k = tmp$end.time != ''
      nice$End[k] = paste(tmp$end.time[k], '<br>Lon:', tmp$end.lon[k], '<br>Lat:', tmp$end.lat[k])

      DT::datatable(nice, editable = F, filter = 'top', rownames = F, selection = 'single', escape = F)
    })


  output$history = renderUI({

    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$events_rows_selected)) {
      tmp = log()
      tab = parseLog()
      record = readLines('log/diagnostics.log')
      add.log(paste('Searching for the history of event', tab$id[input$events_rows_selected], '.'))
      record = record[grepl(tab$id[input$events_rows_selected], record)]

      return(HTML(paste0(record, collapse = '<br />')))
    }
  })


  output$idSummary = renderUI({
    id = idList()
    nex = which(id$all == max(id$used, na.rm = T))
    if (is.na(nex)) {nex = 1}
    res = tagList()
    res[[1]] = tags$text('...')

    for (i in pmax(1, nex - 20):pmin(nex + 20, length(id$all))) {
      if (id$all[i] %in% id$used) {
        res[[length(res) + 1]] = tags$text(id$all[i], style = "color:red")
      } else if (i == nex + 1) {
        res[[length(res) + 1]] = tags$text(id$all[i], style = "color:green")
      } else {
        res[[length(res) + 1]] = tags$text(id$all[i], style = "color:gray")
      }
    }

    res[[length(res) + 1]] = tags$text('...')
    res
  })

  output$eventSummary = renderUI({
    parse = parseLog()
    parse = parse[order(parse$id),]

    res = tagList()

    for (i in 1:nrow(parse)) {
      if (parse$status[i] == 'QUEUED') {
        res[[length(res) + 1]] = tags$text(parse$id[i], style = "color:red")
      } else if (parse$status[i] == 'STARTED') {
        res[[length(res) + 1]] = tags$text(parse$id[i], style = "color:green")
      } else if (parse$status[i] == 'ENDED') {
        res[[length(res) + 1]] = tags$text(parse$id[i], style = "color:white")
      } else {
        res[[length(res) + 1]] = tags$text(parse$id[i], style = "color:gray")
      }
    }

    res
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
                        Event.ID = tmp$id,
                        Sample.IDs = tmp$events,
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
                        Event.ID = tmp$id,
                        Sample.IDs = tmp$events,
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
      tmp = read.csv('log/nmea.csv', header = F)
      if (!settings$demo.mode) {
        openxlsx::write.xlsx(tmp, file)
      }
    })
}

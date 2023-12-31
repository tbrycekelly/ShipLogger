server = function(input, output, session) {

  source('config.R')
  source('functions.R')
  add.log('New session created. Loaded source files.')


  ## Initialize log if does not exist.
  if (!file.exists('log/log.json')) {
    tmp = blank.entry(1)
    tmp$Cruise = settings$cruise
    tmp$Instrument = 'System'
    tmp$Action = 'Start'
    tmp$Notes = 'Initialized ShipLogger.'
    tmp = as.list(tmp)

    writeLines(jsonlite::toJSON(tmp), 'log/log.json')
  }

  if (!file.exists('log/position.csv')) {
    writeLines("time,gps.time,lon,lat", 'log/position.csv')
  }


  log = reactive({
    input$enter
    input$clear.enter
    input$refresh
    input$delete_button
    load.log('log/log.json')
  })


  add.log('Initialization finished.')

  clear = function() {
    add.log('Clearing user input fields.')
    updateTextInput(inputId = "entry", value = "")
    updateSelectInput(inputId = 'author', selected = F)
    updateSelectInput(inputId = 'instrument', selected = F)
    updateTextInput(inputId = "stn", value = "")
    updateTextInput(inputId = "cast", value = "")
    updateTextInput(inputId = "bottom", value = "")
    updateRadioGroupButtons(inputId = 'action', selected = F)
  }


  observeEvent(
    input$clear.enter,
    {
      clear()
    }
  )


  observeEvent(
    input$enter,
    {
      id = digest::digest(Sys.time(), algo = 'crc32')
      add.log(paste('Logging user event', id,'.'))
      file = 'log/log.json'

      entry = list(ID = id,
                  Time = paste0(format(Sys.time())),
                  Lon = position()$lon,
                  Lat = position()$lat,
                  Station = toupper(input$stn),
                  Cast = input$cast,
                  Depth = input$bottom,
                  Instrument = input$instrument,
                  Action = input$action,
                  Author = input$author,
                  Notes = input$entry
                  )

      write.json(filename = 'log/log.json', entry = entry)
      if (!is.null(input$action)) {
        if (input$action %in% settings$final.action) {
          clear()
        } else {
          add.log(paste0('Incremeting action item selection for ', input$instrument, '.'))
          i = which(input$action == instruments[[input$instrument]])
          if (i == length(instruments[[input$instrument]])) {
            clear()
          } else {
            updateRadioGroupButtons(inputId = 'action', selected = instruments[[input$instrument]][i+1])
          }
        }
      }
    })


  ## Preserve edits
  observeEvent(
    input$events_cell_edit,
    {
      row  = input$events_cell_edit$row
      col = input$events_cell_edit$col + 1
      tmp = log()
      tab = parse.log(tmp)

      add.log(paste('Entry modified at (row, col)', row, col,'. Keys are:', names(tab)[col], 'and', tab$ID[row],'. New content: ', input$events_cell_edit$value, '.'))

      for (i in 1:length(tmp)) {
        if (tab$ID[row] == tmp[[i]]$ID) {
          entry = tmp[[i]]
          n = names(tab)[col]
          entry[[n]] = input$events_cell_edit$value
          write.json('log/log.json', entry)
          add.log(paste('Original entry found for ', tab$ID[row], '. Appending updates for parameter', n, 'with value', input$events_cell_edit$value))
          return()
        }
      }
    })

  observeEvent(
    input$edit_button,
    {
      row  = input$events_rows_selected
      add.log(paste('Entry selected for review is ', row, '.'))
      if (length(row) > 0) {
        showModal(

          modalDialog(
            div(
              id = ("entry_form"),
              tags$head(tags$style(".modal-dialog{ width:760px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  p('Revision history:'),
                  htmlOutput('history')
                ),
                easyClose = TRUE
              )
            )
          )
        )
      }
    }
  )

  observeEvent(
    input$delete_button,
    {
      row  = input$events_rows_selected

      if (length(row) == 1) {
        tmp = log()
        tab = parse.log(tmp)

        add.log(paste0('Entry selected for deletion is ', row, ' (', tab$ID[row],').'))
        for (i in 1:length(tmp)) {
          if (tab$ID[row] == tmp[[i]]$ID) {
            entry = tmp[[i]]
            entry[['Action']] = 'Delete'
            write.json('log/log.json', entry)
            clear()
            return()
          }
        }
      }
    })

  observeEvent(
    input$instrument,
    {
      updateRadioGroupButtons(inputId = 'action', choices = instruments[[input$instrument]])
    }
  )

  observeEvent(input$about, {
    shinyalert::shinyalert(title = 'About this app',
                           text = 'This app was created by Tom Kelly (<a target = "_new_" href = "https://github.com/tbrycekelly">Github Link</a>).<br /> To learn more about this app and how to set it up please visit the <a target = "_new_" href = "http://github.com/tbrycekelly/ShipLogger">project page</a>.',
                           html = T)
  })


  position = reactive({
    invalidateLater(settings$nmea.update*1000)

    ## Retreive NMEA feed:
    if (settings$nmea.type == 'serial') {
      tmp = getSerialMessage(settings = settings)
    } else if (settings$nmea.type == 'tcp') {
      tmp = getTCPMessage(settings = settings)
    } else if (settings$nmea.type == 'demo') {
      tmp = getDemoMessage(settings = settings)
    } else {
      add.log('Incorrect NMEA type specified, no data returned.')
      tmp = c()
    }

    # Catch no message condition:
    if (length(tmp) < 1) {
      return(list(lon = 0, lat = 0))
    }

    add.log(paste('Received', length(tmp), ' NMEA sentances from GPS feed.'))
    tmp = strsplit(tmp, ',')

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

    write.table(x = data.frame(time = Sys.time(), gps.time = time.raw, lon = lon, lat = lat), sep = ',', file = 'log/position.csv', append = T, col.names = F, row.names = F)

    list(lon = lon, lat = lat)
  })


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


  output$lon = renderText({
    pos = position()
    s = 'E'
    if (pos$lon < 0) { s = 'W'; pos$lon = -pos$lon}
    paste('Lon:', round(pos$lon, digits = 4), s)
  })


  output$lat = renderText({
    pos = position()
    s = 'N'
    if (pos$lat < 0) { s = 'S'; pos$lat = -pos$lat}
    paste('Lat:', round(pos$lat, digits = 4), s)
  })

  output$events = renderDT(
    {
      tmp = parse.log(log())
      DT::datatable(tmp, editable = T, filter = 'top', rownames = F, selection = 'single')
    })


  output$recent = renderTable(
    {
      invalidateLater(1000*5)

      tmp = parse.log(log())
      tmp = tmp[,c('Time', 'Station', 'Instrument', 'Action')]
      head(tmp, 8)
    })

  output$history = renderUI({

    message(Sys.time(), ': Rendering history.')
    if (!is.null(input$events_rows_selected)) {
      tmp = log()
      tab = parse.log(tmp)
      record = readLines('log/diagnostics.log')

      add.log(paste('Searching for the history of event', tab$ID[input$events_rows_selected], '.'))

      record = record[grepl(tab$ID[input$events_rows_selected], record)]

      return(HTML(paste0(record, collapse = '<br />')))
    }
  })


  #### Download options:

  output$download.csv = downloadHandler(
    filename = function() {
        paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.csv')
      },
    content = function (file) {
      add.log('Preparing csv download.')
      tmp = log()
      tmp = parse.log(tmp)

      write.csv(tmp, file)
    })


  output$download.xlsx = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      add.log('Preparing xlsx download.')
      tmp = log()
      tmp = parse.log(tmp)

      openxlsx::write.xlsx(tmp, file)

    })


  output$download.json = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      add.log('Preparing JSON download.')
      tmp = readLines('log/log.json')
      writeLines(tmp, file)
    })


  output$download.pos = downloadHandler(
    filename = function() {
      paste0('ShipLogger Position Record ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      add.log('Preparing Positions download.')
      tmp = read.csv('log/position.csv')
      openxlsx::write.xlsx(tmp, file)
    })


}

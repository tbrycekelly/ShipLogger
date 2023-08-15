server = function(input, output, session) {

  add.log('New session created. Loading source files.')
  source('config.R')
  source('functions.R')
  add.log('Done loading source files.')


  ## Initialize log if does not exist.
  if (!file.exists('log/log.json')) {
    tmp = blank.entry(1)
    tmp$Instrument = 'System'
    tmp$Action = 'Start'
    tmp$Notes = 'Initialized ShipLogger.'
    tmp = as.list(tmp)

    writeLines(jsonlite::toJSON(tmp), 'log/log.json')
  }
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

      if (input$action == settings$final.action) {
        clear()
      } else {
        add.log('Incremeting action item selection.')
        i = which(input$action == actions)
        updateRadioGroupButtons(inputId = 'action', selected = actions[i+1])
      }
    })


  ## Preserve edits
  observeEvent(
    input$events_cell_edit,
    {
      row  <- input$events_cell_edit$row
      col <- input$events_cell_edit$col
      add.log(paste('Entry modified at', row, col,'.'))
      tmp = load.log('log/log.json')
      tab = parse.log(tmp)
      for (i in length(tmp):1) {
        if (tab$ID[row] == tmp[[i]]$ID) {

          entry = tmp[[i]]
          n = names(tab)[col]
          entry[[n]] = input$events_cell_edit$value
          write.json('log/log.json', entry)
          add.log(paste('Original entry found. Appending updates for parameter', n, 'with value', input$events_cell_edit$value))
          return()
        }
      }
    })



  position = reactive({
    invalidateLater(settings$nmea.update)

    tmp = c()
    tryCatch({
      gps = socketConnection(host = settings$nmea.host, port = settings$nmea.port)
      Sys.sleep(2)
      tmp = readLines(gps, n = 30)
      close(gps)

      tmp = tmp[grepl('GPGGA', tmp)]
    }, error = function(e) {
      add.log('Unable to parse GPS feed. Check NMEA settings if problem persists.')
    }, warning = function(w) {
      # Do nothing.
    }
    )


    if (length(tmp) > 0) {
      add.log(paste('Received', length(tmp), ' NMEA sentances from GPS feed.'))
      tmp = strsplit(tmp, ',')

      lat.raw = tmp[[1]][3]
      lon.raw = tmp[[1]][5]

      lat = as.numeric(substr(lat.raw, 1, nchar(lat.raw) - 7)) + as.numeric(substr(lat.raw, nchar(lat.raw)-6, nchar(lat.raw)))/60
      lon = as.numeric(substr(lon.raw, 1, nchar(lon.raw) - 7)) + as.numeric(substr(lon.raw, nchar(lon.raw)-6, nchar(lon.raw)))/60
    } else {
      lat = NA
      lon = NA
    }

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
      paste(format(Sys.time()-8*3600), ' (local)')
    })


  output$lon = renderText(
    paste0('Lon: ', round(position()$lon, digits = 4), ' W')
  )


  output$lat = renderText(
    paste0('Lat: ', round(position()$lat, digits = 4), ' N')
  )

  output$events = renderDT(
    {
      tmp = load.log('log/log.json')
      DT::datatable(parse.log(tmp), editable = T)
    })


  output$recent = renderTable(
    {
      invalidateLater(1000*5)

      tmp = load.log('log/log.json')
      tmp = parse.log(tmp)

      tmp = tmp[,c('Time', 'Station', 'Instrument', 'Action')]
      head(tmp, 8)
    })



  #### Download options:

  output$download.csv = downloadHandler(
    filename = function() {
        paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.csv')
      },
    content = function (file) {
      add.log('Preparing csv download.')
      tmp = load.log('log/log.json')
      tmp = parse.log(tmp)

      write.csv(tmp, file)
    })


  output$download.xlsx = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      add.log('Preparing xlsx download.')
      tmp = load.log('log/log.json')
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


}

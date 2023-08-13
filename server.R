server = function(input, output, session) {

  message(Sys.time(), ': User Connected. Loading config file.', appendLF = F)
  source('config.R')
  message(' Done.')

  ## Initialize log if does not exist.
  if (!file.exists('log/log.json')) {
    tmp = blank.entry(1)
    tmp$Instrument = 'System'
    tmp$Action = 'Start'
    tmp$Notes = 'Initialized ShipLogger.'

    writeLines(jsonlite::toJSON(tmp), 'log/log.json')
  }

  ## Take log entry message and write it to JSON log file.
  write.json = function(filename, entry) {

    dat = jsonlite::toJSON(entry)

    file.copy(from = filename, to = paste0(filename, ' ', gsub(':', '', Sys.time()), '.old'))

    ## Write entry.
    f = file(description = filename, open = 'a')
    writeLines(dat, con = f)
    close(f)

    shiny::showNotification(ui = 'Event Logged', duration = 5, type = 'message')
  }


  clear = function() {
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

  ## Parse json log entries into data.frame
  parse.log = function(json) {
    log = blank.entry(length(json))

    for (i in 1:length(json)) {
      for (n in names(json[[i]])) {
        if (n %in% names(log) & length(json[[i]][[n]]) > 0) {
          log[i,n] = json[[i]][[n]]
        }
      }
    }

    log
  }


  observeEvent(
    input$enter,
    {
      id = digest::digest(Sys.time(), algo = 'crc32')
      message(Sys.time(), ': Logging CTD Event ', id)
      file = 'log/log.json'

      if (!file.exists(file)) {
        file.create(file)
      }

      ## Format Data
      # "Datetime; Cast; Station; Depth; Instrument; Action; Author; Note;"
      entry = list(ID = id,
                  Time = format(Sys.time()),
                  Station = toupper(input$stn),
                  Cast = input$cast,
                  Depth = input$bottom,
                  Instrument = input$instrument,
                  Action = input$action,
                  Author = input$author,
                  Notes = input$entry
                  )

      ## Write to main log:
      write.json(filename = 'log/log.json', entry = entry)

      clear()
    })



  #### Outputs

  output$currentTime = renderText({
    invalidateLater(1000, session)
    paste(format(Sys.time()), ' (system)')
  })


  output$currentTime.local = renderText({
    invalidateLater(1000, session)
    paste(format(Sys.time()-8*3600), ' (local)')
  })

  output$events = renderDataTable({
    tmp = readLines('log/log.json')
    tmp = lapply(tmp, jsonlite::fromJSON)
    tmp = parse.log(tmp)

    tmp
  })


  output$recent = renderTable({
    invalidateLater(1000*5)

    tmp = readLines('log/log.json')
    tmp = lapply(tmp, jsonlite::fromJSON)
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
      tmp = readLines('log/log.json')
      tmp = lapply(tmp, jsonlite::fromJSON)
      tmp = parse.log(tmp)

      write.csv(tmp, file)
    })


  output$download.xlsx = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.xlsx')
    },
    content = function (file) {
      tmp = readLines('log/log.json')
      tmp = lapply(tmp, jsonlite::fromJSON)
      tmp = parse.log(tmp)

      openxlsx::write.xlsx(tmp, file)

    })


  output$download.json = downloadHandler(
    filename = function() {
      paste0('ShipLogger ', gsub(':', '', format(Sys.time())), '.json')
    },
    content = function (file) {
      tmp = readLines('log/log.json')
      writeLines(tmp, file)
    })


}

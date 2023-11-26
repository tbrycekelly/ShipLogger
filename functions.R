library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)


#' Programmatically create a Shiny input
#'
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
shinyInput <- function(FUN, id, ...) {

  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(1:length(id), function(i){
    as.character(FUN(paste0(id[i]), ...))
  }, character(1))
}

init.log = function(path = 'log/log.json') {
  if (!file.exists('log/log.json')) {
    tmp = as.list(blank.event())
    tmp$id = settings$event.ids[1]
    tmp$events = NA
    tmp$instrument = 'System'
    tmp$status = 'ENDED'
    tmp$notes = 'Initialized ShipLogger.'

    writeLines(jsonlite::toJSON(tmp), 'log/log.json')
  }
}

blank.event = function(n = 1) {

  data.frame(
    id  = sapply(runif(n), digest::digest),
    events = '',
    n = 0,
    status = 'QUEUED',
    start.time = '',
    start.lon = '',
    start.lat = '',
    end.time = '',
    end.lon = '',
    end.lat = '',
    station = '',
    cast = '',
    instrument = '',
    author = '',
    notes = ''
  )
}



## Take log entry message and write it to JSON log file.
write.json = function(filename, entry) {
  if (settings$demo.mode) {
    return()
  }
  entry = as.list(entry)

  dat = jsonlite::toJSON(entry)

  file.copy(from = filename, to = paste0(filename, ' ', gsub(':', '', Sys.time()), '.old'))

  # Write entry.
  f = file(description = filename, open = 'a')
  writeLines(dat, con = f)
  close(f)

  shiny::showNotification(ui = 'Event Logged', duration = 5, type = 'message')
}




## Parse json log entries into data.frame
parse.log = function(json, all = F) {
  log = blank.event(length(json))

  for (i in 1:length(json)) {
    for (n in names(json[[i]])) {
      if (n %in% names(log) & length(json[[i]][[n]]) == 1) {
        log[i,n] = json[[i]][[n]]
      }
    }
  }


  ## Remove edits and deletions. Then order
  log$id = as.numeric(log$id)
  if (!all) {
    log = log[!duplicated(log$id) & !is.na(log$id),]
  }
  log = log[order(log$id, decreasing = T),]

  log
}


load.log = function(file = 'log/log.json') {
  tmp = readLines(file)
  tmp = tmp[length(tmp):1] # rev chronology
  lapply(tmp, function(x) {jsonlite::fromJSON(x, simplifyDataFrame = F)})
}


add.log = function(message, file = 'log/diagnostics.log') {
  if (settings$demo.mode) {
    return()
  }
  f = file(file, open = 'a')
  writeLines(paste0(Sys.time(), ': ', message), con = f)
  writeLines(paste0(Sys.time(), ': ', message))
  close(f)
}


recordNMEA = function(settings) {

  ## Setup connection
  if (settings$nmea.type == 'serial') {
    con = serial::serialConnection(port = settings$nmea.serial.port, mode = settings$nmea.serial.mode)
    open(con)
  } else if (settings$nmea.type == 'tcp') {
    con = socketConnection(host = settings$nmea.tcp.host, port = settings$nmea.tcp.port)
  } else {
    ## do nothing for demo.
  }

  ## Run indefinitely
  while (T) {
    if (settings$nmea.type == 'serial') {
      raw = getSerialMessage(settings, con)
    } else if (settings$nmea.type == 'tcp') {
      raw = getTCPMessage(settings, con)
    } else {
      raw = getDemoMessage(settings)
    }

    raw = strsplit(raw, '\\$')[[1]]

    f = file('log/nmea.csv', open = 'a')
      writeLines(paste0(Sys.time(), '; ', raw), con = f)
      writeLines(paste0(Sys.time(), '\t', raw))
      close(f)
      raw = raw[grepl('GGA', toupper(raw))] # Filter for positions

      tryCatch({
        saveRDS(list(time = Sys.time(), sentance = raw), 'log/last.rds')
      }, error = function(e) {
        add.log(paste('Unable to write to last.rds. If problem persists, check that file is not locked. Error information:', e))
      }, warning = function(w) {
        add.log(paste('Warning when writing to last.rds. If problem persists, check that file is not locked. Warning information:', w))
      })

    Sys.sleep(settings$nmea.update)
  }
}


getSerialMessage = function(settings, con) {
  tmp = c()
  tryCatch({
    tmp = serial::read.serialConnection(con = con, n = 0)

    #tmp = tmp[grepl('GGA', toupper(tmp))]
  }, error = function(e) {
    add.log(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', e), file = 'log/nmea.log')
  }, warning = function(w) {
    add.log(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', w), file = 'log/nmea.log')
  })

  tmp
}


getTCPMessage = function(settings, con) {
  tmp = c()
  tryCatch({
    tmp = readLines(con, n = -1)

    #tmp = tmp[grepl('GGA', toupper(tmp))]
  }, error = function(e) {
    add.log(paste('Unable to parse GPS feed from TCP connection. Check NMEA settings if problem persists.'), file = 'log/nmea.log')
  }, warning = function(w) {
    # Do nothing.
  }
  )

  tmp
}


getDemoMessage = function(settings, delay = 2) {
  paste0('$GPGGA,123519,', 4807.038 + round(runif(1), 3), ',N,', round(01131.000 + runif(1), 3), ',E,1,08,,545.440,M,,,,*47')
}




shinyInput = function(FUN, id, ...) {

  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(id, function(i){
    as.character(FUN(i, ...))
  }, character(1))

}



#Label mandatory fields
labelMandatory = function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS = ".mandatory_star { color: red; }"



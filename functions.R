library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)


blank.entry = function(n) {
  data.frame(
    ID  = rep(digest::digest(Sys.time(), algo = 'crc32'), n),
    Time = NA,
    Lon = NA,
    Lat = NA,
    Station = NA,
    Cast = NA,
    Instrument = NA,
    Action = NA,
    Author = NA,
    Notes = NA
  )
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

  ## Remove edits and deletions. Then order
  log = log[!duplicated(log$ID),]
  log = log[log$Action != 'Delete',]
  log = log[order(log$Time, decreasing = T),]

  log
}


load.log = function(file = 'log/log.json') {
  tmp = readLines(file)
  tmp = tmp[length(tmp):1] # rev chronology
  lapply(tmp, function(x) {jsonlite::fromJSON(x, simplifyDataFrame = F)})
}


add.log = function(message, file = 'log/diagnostics.log') {
  f = file(file, open = 'a')
  writeLines(paste0(Sys.time(), ': ', message), con = f)
  writeLines(paste0(Sys.time(), ': ', message))
  close(f)
}


getSerialMessage = function(settings) {
  tmp = c()
  tryCatch({
    tmp = serial::read.serialConnection(con = con, n = 0)

    tmp = tmp[grepl('GGA', toupper(tmp))]
  }, error = function(e) {
    add.log(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', e))
  }, warning = function(w) {
    add.log(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', w))

  })

  tmp
}


getTCPMessage = function(settings, delay = 2) {
  tmp = c()
  tryCatch({
    gps = socketConnection(host = settings$nmea.tcp.host, port = settings$nmea.tcp.port)
    Sys.sleep(delay)
    tmp = readLines(gps, n = 100)
    close(gps)

    tmp = tmp[grepl('GGA', toupper(tmp))]
  }, error = function(e) {
    add.log(paste('Unable to parse GPS feed from TCP connection. Check NMEA settings if problem persists.'))
  }, warning = function(w) {
    # Do nothing.
  }
  )

  tmp
}


getDemoMessage = function(settings, delay = 2) {
  paste0('$GPGGA,123519,', 4807.038 + round(runif(1), 3), ',N,', round(01131.000 + runif(1), 3), ',E,1,08,,545.440,M,,,,*47')
}


getNMEAFile = function(settings) {
  tmp = c()
  tryCatch({
    if (dir.exists(settings$nmea.file)) {
      add.log('NMEA file is directory. Attempting to get newest file.')
      path = list.files(settings$nmea.file, full.names = T, pattern = settings$nmea.file.pattern)
      mtime = do.call(file.info, as.list(path))$mtime
      l = which.max(mtime)
      if (as.numeric(difftime(mtime[l], Sys.time(), units = 'mins')) > 5) {
        add.log('NMEA file is too old (>5 min), rejecting data.')
        return(tmp)
      }
      path = path[l]
    } else if (file.exists(settings$nmea.file)) {
      path = settings$nmea.file
    } else {
      return(tmp)
    }

    tmp = readLines(path[l])
    tmp = tmp[grepl('GGA', toupper(tmp))] # INGGA and GPGGA sentences. TODO: toupper()?
    if (settings$nmea.file.order == 'asc') {
      tmp = rev(tmp)
    }

  }, error = function(e) {
    add.log('Unable to parse GPS feed. Check NMEA settings if problem persists.')
  }, warning = function(w) {
    # Do nothing.
  }
  )

  tmp[1]
}


testNMEA = function(settings, delay = 2) {
  message('Testing NMEA settings:')

  if (settings$nmea.type == 'serial') {
    if (!toupper(settings$nmea.serial.port) %in% toupper(serial::listPorts())) {
      message('N.B. Specified serial port does not appear to exist. Attempting anyway.')
    }
    tmp = getSerialMessage(settings = settings, delay = delay)
  } else if (settings$nmea.type == 'tcp') {
    tmp = getTCPMessage(settings = settings, delay = delay)
  } else {
    add.log('Incorrect NMEA type specified, no data returned.')
    tmp = c()
  }

  message('Retreived ', length(tmp), ' NMEA sentences:\n', paste('\t', tmp, collapse = '\n'))
}

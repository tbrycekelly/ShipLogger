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
    Cruise = 'current cruise',
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

  ## Remove edits
  log = log[nrow(log):1,]
  log = log[!duplicated(log$ID),]

  log
}


load.log = function(file = 'log/log.json') {
  tmp = readLines(file)
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
    con = serial::serialConnection(port = settings$nmea.serial.port, mode = settings$nmea.serial.mode)
    Sys.sleep(2)
    tmp = serial::read.serialConnection(con = con, n = 0)
    close(con)
  }, error = function(e) {
    add.log(paste('Unable to retreive GPS feed. Check NMEA settings if problem persists. Error information:', e))
  }, warning = function(w) {

  })

  tmp
}


getTCPMessage = function(settings) {
  tmp = c()
  tryCatch({
    gps = socketConnection(host = settings$nmea.tcp.host, port = settings$nmea.tcp.port)
    Sys.sleep(2)
    tmp = readLines(gps, n = 100)
    close(gps)

    tmp = tmp[grepl('GPGGA', tmp)]
  }, error = function(e) {
    add.log('Unable to parse GPS feed. Check NMEA settings if problem persists.')
  }, warning = function(w) {
    # Do nothing.
  }
  )

  tmp
}


testNMEA = function(settings) {
  message('Testing NMEA settings:')

  if (settings$nmea.type == 'serial') {
    if (!toupper(settings$nmea.serial.port) %in% toupper(serial::listPorts())) {
      message('N.B. Specified serial port does not appear to exist. Attempting anyway.')
    }
    tmp = getSerialMessage(settings = settings)
  } else if (settings$nmea.type == 'tcp') {
    tmp = getTCPMessage(settings = settings)
  } else {
    add.log('Incorrect NMEA type specified, no data returned.')
    tmp = c()
  }

  message('Retreived ', length(tmp), ' NMEA sentences:\n', paste('\t', tmp, collapse = '\n'))
}

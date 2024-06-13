library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)
library(SimpleMapper)



parseNMEA = function(str) {
  if (sum(nchar(str)) < 30) {
    return(list(lon = NA, lat = NA))
  }
  tmp = strsplit(str, split = ',')

  n = max(length(str) - 1, 1)

  if (nchar(str[[n]]) < 30) {
    return(list(lon = NA, lat = NA))
  }

  time.raw = tmp[[n]][2]
  lat = tmp[[n]][3]
  lon = tmp[[n]][5]
  north = toupper(tmp[[n]][4]) == 'N'
  east = toupper(tmp[[n]][6]) == 'E'

  latdegree = floor(as.numeric(lat)/100)
  lat = latdegree + (as.numeric(lat) - latdegree*100)/60
  londegree = floor(as.numeric(lon)/100)
  lon = londegree + (as.numeric(lon) - londegree*100)/60

  if (!north) {
    lat = -lat
  }
  if (!east) {
    lon = -lon
  }
  list(lon = lon, lat = lat, gpstime = time.raw)
}


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


QueuedRecord = function(path = 'log/log.rds') {
  if (!file.exists(path)) {
    tmp = blank.event()
    tmp[[1]]$event = 0
    tmp[[1]]$instrument = 'System'
    tmp[[1]]$status = 'ENDED'
    tmp[[1]]$notes = 'Queuedialized ShipLogger.'

    saveRDS(tmp, path)
  }
}


blank.event = function(n = 1) {
  tmp = list()
  for (i in 1:n) {
    id = digest::digest(runif(1))
    tmp[[id]] = list(
      id = id,
      instrument = '',
      author = '',
      station = '',
      cast = '',
      status = 'Queued',
      events = list(
        Queued = list(
          status = 'Queued',
          time = Sys.time(),
          longitude = NA,
          latitude = NA)
      ),
      depth = NA,
      notes = ''
    )
  }

  tmp
}


addLog = function(message) {
  print(message)
}


## Take log entry message and write it to JSON log file.
appendRecord = function(entry, path = 'log/log.rds') {
  newpath = paste0(path, ' ', gsub(':', '', Sys.time()), '.old')
  if (file.copy(from = path, to = newpath)) {
    tmp = readRDS(newpath)
    tmp[[entry$id]] = entry
    saveRDS(tmp, path)

    shiny::showNotification(ui = 'Event Logged', duration = 5, type = 'message')
  } else {
    shiny::showNotification(ui = 'Could not create log backup!', duration = 5, type = 'error')
  }
}


flattenLog = function(record) {

  count = 0
  for (i in 1:length(record)) {
    count = count + length(record[[i]]$events)
  }

  table = data.frame(id = rep(NA, count), instrument = NA, author = NA, station = NA, cast = NA, status = NA, time = NA, longitude = NA, latitude = NA, depth = NA, notes = NA)

  for (i in 1:length(record)) {
    if (length(record[[i]]$events) > 0) {
      for (j in 1:length(record[[i]]$events)) {
        table$id[count] = record[[i]]$id
        table$instrument[count] = record[[i]]$instrument
        table$author[count] = record[[i]]$author
        table$station[count] = record[[i]]$station
        table$cast[count] = record[[i]]$cast
        table$status[count] = record[[i]]$events[[j]]$status
        table$time[count] = record[[i]]$events[[j]]$time
        table$longitude[count] = record[[i]]$events[[j]]$longitude
        table$latitude[count] = record[[i]]$events[[j]]$latitude
        table$depth[count] = record[[i]]$depth
        table$notes[count] = record[[i]]$notes

        count = count - 1
      }
    }
  }

  table
}


## Parse json log entries into data.frame
parseLog = function(record, all = F) {
  tmp = flattenLog(record)
  if (!all) {
    #tmp = tmp[!duplicated(tmp$id),] # Remove duplicated entries
    l = unique(tmp$id[tmp$status == 'DELETED'])
    tmp = tmp[!tmp$id %in% l,]
  }
  tmp
}




### Start NMEA stuff

recordNMEA = function(settings) {

  ## Setup connection
  # Do this block one time to Queuedialize & test feeds.
  if (settings$nmea.type == 'serial') {
    con = serial::serialConnection(name = 'nmeafeed', port = settings$nmea.serial.port, mode = settings$nmea.serial.mode)
    open(con)
  } else if (settings$nmea.type == 'tcp') {
    con = socketConnection(host = settings$nmea.tcp.host, port = settings$nmea.tcp.port)
  } else {
    ## do nothing for demo.
  }

  if (!file.exists('log/position.csv')) {
    write('lon,lat', file = 'log/position.csv')
  }
  Sys.sleep(settings$nmea.update) # for good measure, wait 1 update period.

  ## Run indefQueuedely
  # Main body of the script which should just run indefQueuedely.
  while (T) {
    if (settings$nmea.type == 'serial') {
      raw = getSerialMessage(settings, con)
    } else if (settings$nmea.type == 'tcp') {
      raw = getTCPMessage(settings, con)
    } else {
      raw = getDemoMessage(settings)
    }

    raw = strsplit(raw, '\\$')[[1]]

    ## Archive and start new lof
    if (file.exists('log/nmea.csv')) {
      if (file.size('log/nmea.csv') > 2e7) {
        file.rename('log/nmea.csv', paste0('log/nmea ', gsub(':', '', Sys.time()), '.csv'))
      }
    }

    f = file('log/nmea.csv', open = 'a')
    writeLines(paste0(Sys.time(), '; ', raw), con = f)
    writeLines(paste0(Sys.time(), '\t', raw))
    close(f)
    raw = raw[grepl('GGA', toupper(raw))] # Filter for positions

    tryCatch({
      saveRDS(list(time = Sys.time(), sentance = raw), 'log/last.rds')
    }, error = function(e) {
      addLog(paste('Unable to write to last.rds. If problem persists, check that file is not locked. Error information:', e))
    }, warning = function(w) {
      addLog(paste('Warning when writing to last.rds. If problem persists, check that file is not locked. Warning information:', w))
    })

    tryCatch({
      pos = parseNMEA(raw)
      write(paste0(pos$lon, ',', pos$lat), file = 'log/position.csv', append = T)
    }, error = function(e) {
      addLog(paste('Unable to write to position.csv If problem persists, check that file is not locked. Error information:', e))
    }, warning = function(w) {
      addLog(paste('Warning when writing to position.csv If problem persists, check that file is not locked. Warning information:', w))
    })

    Sys.sleep(settings$nmea.update)
  }
}


getSerialMessage = function(settings, con) {
  tmp = c()
  tryCatch({
    tmp = serial::read.serialConnection(con = con, n = 0)
  }, error = function(e) {
    addLog(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', e), file = 'log/nmea.log')
  }, warning = function(w) {
    addLog(paste('Unable to retreive GPS feed from serial connection. Check NMEA settings if problem persists. Error information:', w), file = 'log/nmea.log')
  })

  tmp
}


getTCPMessage = function(settings, con) {
  tmp = c()
  tryCatch({
    tmp = readLines(con, n = -1)
  }, error = function(e) {
    addLog(paste('Unable to parse GPS feed from TCP connection. Check NMEA settings if problem persists.'), file = 'log/nmea.log')
  }, warning = function(w) {
    # Do nothing.
  }
  )

  tmp
}


getDemoMessage = function(settings, delay = 2) {
  paste0('$GPGGA,123519,', 4807.038 + round(runif(1, 0, 500), 3), ',N,', round(01131.000 + rnorm(1, 0, 100), 3), ',W,1,08,,545.440,M,,,,*47')
}




inactivity = "function idleTimer() {
  var t = setTimeout(logout, 600000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.location.reload();;  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"




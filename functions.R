library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)
library(SimpleMapper)


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


isoTime = function(x, rev = F) {

  if (rev) {
    return(format(x, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC'))
  }

  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
}



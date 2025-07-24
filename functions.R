library(shiny)
library(shinyWidgets)
library(jsonlite)
library(data.table)
library(DT)
library(serial)
library(openxlsx)
library(SimpleMapper)



addLog = function(msg, level = 'DEBUG') {
  msg = paste0(isoTime(), '\t[', level, ']\t', msg)
  message(msg)
}

isoTime = function(x = Sys.time(), rev = F) {

  if (rev) {
    return(format(x, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC'))
  }

  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = 'UTC')
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


eventTemplate = function(n = 1) {
  tmp = data.frame(
    event_id = rep(NA, n), # links all records together
    group_id = NA,
    instrument = '',
    action = 'Queue',
    author = '',
    station = '',
    cast = '',
    transect = '',
    maximum_depth = NA,
    bottom_depth = NA,
    notes = '',
    alive = TRUE,
    datetime = Sys.time(),
    longitude = NA,
    latitude = NA
  )
  for (i in 1:n) {
    tmp$group_id[i] = ulid()
    tmp$event_id[i] = ulid()
  }

  tmp
}

#Label mandatory fields
labelMandatory = function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS = ".mandatory_star { color: red; }"


initShiplogger = function() {

  if (!file.exists(settings$databaseFile)) {
    con = dbConnect(RSQLite::SQLite(), settings$databaseFile)

    # Create a table with a TEXT primary key
    dbExecute(con, "
    CREATE TABLE IF NOT EXISTS events (
      event_id TEXT PRIMARY KEY,
      group_id TEXT,
      instrument TEXT,
      action TEXT,
      author TEXT,
      station TEXT,
      cast TEXT,
      transect TEXT,
      maximum_depth REAL,
      bottom_depth REAL,
      notes TEXT,
      alive BOOLEAN,
      datetime REAL,
      longitude REAL,
      latitude REAL
    )
  ")
    dbDisconnect(con)

    event = eventTemplate()
    event$instrument = 'ShipLogger'
    event$action = 'Started'
    event$alive = F

    updateRecord(event)
  }
}

## Take log entry message and write it to  log file.
updateRecord = function(updatedRecord) {

  params = as.list(updatedRecord)

  con = dbConnect(RSQLite::SQLite(), settings$databaseFile)
  dbExecute(con,
  "INSERT OR REPLACE INTO events (event_id, group_id, instrument, action, author, station, cast, transect,
    maximum_depth, bottom_depth, notes, alive, datetime, longitude, latitude) VALUES (:event_id, :group_id, :instrument,
  :action, :author, :station, :cast, :transect, :maximum_depth, :bottom_depth, :notes, :alive, :datetime, :longitude, :latitude);",
  params = params
  )
  dbDisconnect(con)
  shiny::showNotification(ui = paste0('Event Logged.\nTransaction recorded to database at ', format(Sys.time(), format = settings$datetime.format)),
                          duration = 5,
                          type = 'message')
}

readRecord = function(path = 'log/log.rds', group_id = NULL) {
  con = dbConnect(RSQLite::SQLite(), settings$databaseFile)
  if (!is.null(group_id)) {
    result = dbGetQuery(con, "SELECT * FROM events WHERE group_id = :group_id", params = list(group_id = group_id))
  } else {
    result = dbGetQuery(con, "SELECT * FROM events")
  }
  result
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

getAvailableActions = function(instrument, performedActions = NULL) {
  actions = c()
  for (tmpAction in instruments[[instrument]]) { # Find all actions that haven't been used or are allowed to be used more than once.
    if (!(tmpAction %in% performedActions) | !(buttons[[tmpAction]]$once)) {
      actions = c(actions, tmpAction)
    }
  }

  actions
}

addEventButtons = function(tmp, all = FALSE) {
  tmp$button = ''
  unseenID = unique(tmp$group_id)

  for (i in 1:nrow(tmp)) {
    if (tmp$group_id[i] %in% unseenID) {
      unseenID = unseenID[unseenID != tmp$group_id[i]]

      if (tmp$instrument[i] %in% names(instruments) & (tmp$alive[i] | all)) {
        groupMembers = which(tmp$group_id == tmp$group_id[i])
        availableActions = getAvailableActions(instrument = tmp$instrument[i],
                                               performedActions = tmp$action[groupMembers])

        ## Add available action buttons:
        for (action in availableActions) {
          k = which(action == instruments[[tmp$instrument[i]]])
          tmp$button[i] = paste(tmp$button[i],
                                shinyInput(FUN = actionButton,
                                           id = paste0(tmp$group_id[i], '-', k), # <group_id>-<actionNum>
                                           label = action,
                                           onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})',
                                           style=paste0("color: ", buttons[[action]]$text, "; background-color: ", buttons[[action]]$bkg, "; border-color: #2e6da4")
                                )
          )
        }
      }
    }
  }

  ## return
  tmp$button
}

addNoteButtons = function(tmp) {
  ## Add button for notes
  tmp$notebutton = ''
  for (i in 1:nrow(tmp)) {
    if (nchar(tmp$notes[i]) > 0) {
      sign = ' &#9733;'
    } else {
      sign = ''
    }
    tmp$notebutton[i] = paste0(
      shinyInput(
        FUN = actionButton,
        id = paste0(tmp$event_id[i], '-0'),
        label = 'Notes',
        onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})',
        style="color: #fff; background-color: #444; border-color: #2e6da4"
      ),
      sign
    )
    }

  ## return
  tmp$notebutton
}


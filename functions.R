

blank.entry = function(n) {
  data.frame(
    ID  = rep(digest::digest(Sys.time(), algo = 'crc32'), n),
    Cruise = 'current cruise',
    Time = NA,
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

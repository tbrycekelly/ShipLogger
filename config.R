## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.


settings = list(
  nmea.type = 'demo', # Options: 'serial' or 'tcp' or 'demo'
  nmea.tcp.host = "127.0.0.1",
  nmea.tcp.port = 1000,
  nmea.serial.port = "com10",
  nmea.serial.mode = "9600,n,8,1", # Set baud rate here, typically either 9600 or 4600
  nmea.update = 2, #sec
  final.action = c('Recover', 'End'),
  local.timezone = -8,
  event.ids = c(0:500),
  sample.ids = c(10050:18000) # include 1 dummy id for initialization.
)


instruments = c('Trace Metal CTD', 'Standard CTD', 'Trace Metal Fish', 'Ice Camp', 'McLane Pump', 'Surface Pump', 'Underway Sample')

authors = c(
  '',
  'Person1',
  'Person2',
  'Person3'
)

authors = authors[order(authors)]


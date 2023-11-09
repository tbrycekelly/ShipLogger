## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.


settings = list(
  cruise = 'cruise name',
  nmea.type = 'demo', # Options: 'serial' or 'tcp' or 'file'
  nmea.tcp.host = "127.0.0.1",
  nmea.tcp.port = 1000,
  nmea.serial.port = "com10",
  nmea.serial.mode = "9600,n,8,1", # Set baud rate here, typically either 9600 or 4600
  nmea.file = '',
  nmea.file.order = 'asc', # asc = newest entry at end of file; dec = newest entry at start of file.
  nmea.file.pattern = '*',
  nmea.update = 10, #sec
  final.action = c('Recover', 'End'),
  local.timezone = -8,
  event.ids = c(1:500),
  sampe.ids = c(10050:18000)
)


instruments = c('Trace Metal CTD', 'Standard CTD', 'Trace Metal Fish', 'Ice Camp', 'McLane Pump', 'Surface Pump', 'Underway Sample')

authors = c(
  '',
  'Person1',
  'Person2',
  'Person3'
)

authors = authors[order(authors)]


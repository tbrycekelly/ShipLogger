## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.
# Computer should be in UTC, because that is the only responsible thing to do.

settings = list(
  nmea.type = 'demo', # Options: 'serial' or 'tcp' or 'demo'
  nmea.tcp.host = "127.0.0.1",
  nmea.tcp.port = 1000,
  nmea.serial.port = "com10",
  nmea.serial.mode = "9600,n,8,1", # Set baud rate here, typically either 9600 or 4600
  nmea.update = 5, # [seconds] 5 to 10 works well.
  local.timezone = -8, # hour offset from computer time.
  demo.mode = F
)


color = list(
  'Deploy' = list(text = '#ddd', bkg = '#224422'),
  'Recover' = list(text = '#ddd', bkg = '#000'),
  'AtDepth' = list(text = '#ddd', bkg = '#000'),
  'Start' = list(text = '#ddd', bkg = '#000'),
  'End' = list(text = '#ddd', bkg = '#000'),
  'Collected' = list(text = '#ddd', bkg = '#000')
)

instruments = list(
  'CTD' = c('Deploy', 'Recover'),
  'Bongo' = c('Deploy', 'AtDepth', 'Recover'),
  'Multinet' = c('Deploy', 'AtDepth', 'Recover'),
  'Mooring Deployment' = c('Start', 'End'),
  'Mooring Recovery' = c('Start', 'End'),
  'Underway Sample' = c('Collected'),
  'TM Fish' = c('Start', 'End'),
  'TM CTD' = c('Deploy', 'Recover'),
  'CTD (prod)' = c('Deploy', 'Recover'),
  'Other (see note)' = c()
)
instruments = instruments[order(names(instruments))]

authors = c(
  '',
  'Thomas Kelly <tbkelly@alaska.edu>',
  'Person2 <other@email.com>',
  'Pet Shrimp <pelagiclyfe@gmail.com>'
)

authors = authors[order(authors)]


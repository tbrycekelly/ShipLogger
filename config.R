## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.


settings = list(
  cruise = 'cruise name',
  nmea.type = 'tcp', # Options: 'serial' or 'tcp' or 'file'
  nmea.tcp.host = "1.2.3.4",
  nmea.tcp.port = 1000,
  nmea.serial.port = "com5",
  nmea.serial.mode = "9600,n,8,1",
  nmea.file = '',
  nmea.file.order = 'asc', # asc = newest entry at end of file; dec = newest entry at start of file.
  nmea.file.pattern = '*',
  nmea.update = 10, #sec
  final.action = c('Recover', 'End'),
  local.timezone = -8
)


## instrument List
# A list of instruments available in the drop down menu along with associated actions (in order)
# The order of actions is important. The selection will auto increment from the first one until
# the final.action is logged (as set above in settings). When this happens all user input will
# be cleared.
instruments = list()

instruments[['Bongo Net']] =    c('Deploy', 'At Depth', "Recover", 'Abort')
instruments[["Calvet Net"]] =   c('Deploy', "Recover", 'Abort')
instruments[['CTD']] =          c('Deploy', "Recover", 'Abort', 'At Depth')
instruments[['DPI']] =          c('Deploy', "Recover", 'Abort')
instruments[['Iron Fish']] =    c('Deploy', "Recover", 'Abort')
instruments[['Methot']] =       c('Deploy', 'At Depth', "Recover", 'Abort')
instruments[['Mooring Deployment']] = c('Start', "End", 'Abort')
instruments[['Mooring Recovery']] = c('Start', "End", 'Abort')
instruments[['MultiNet']] =     c('Deploy', 'At Depth', "Recover", 'Abort')
instruments[['Sediment Trap']] = c('Deploy', "Recover", 'Abort')
instruments[['TM CTD']] =       c('Deploy', "Recover", 'Abort', 'At Depth')
instruments[['Vertical MultiNet']] = c('Deploy', "Recover", 'Abort')


authors = c(
  '',
  'Person1',
  'Person2',
  'Person3'
)

authors = authors[order(authors)]


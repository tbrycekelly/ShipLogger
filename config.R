## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.
# Computer should be in UTC, because that is the only responsible thing to do.

settings = list(
  datetime.format = '%Y-%m-%d  %H:%M:%S %Z',
  timeouts = list(
    logRefresh = 2, #sec
    pageRefresh = 600, #sec
    positionUpdate = 5, #sec
    uiRefresh = 1, #sec
    mapRefresh = 60 #sc
  ),
  counters = list(
    positionCounter = 10
  ),
  drawIsobath = TRUE,
  databaseFile = 'log/events.db'
)


buttons = list(
  'Deploy' = list(text = '#ddd', bkg = '#224422', terminal = F, once = T),
  'Recover' = list(text = '#ddd', bkg = '#000', terminal = T, once = T),
  'AtDepth' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'Start' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'End' = list(text = '#ddd', bkg = '#000', terminal = T, once = T),
  'Collect' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'Abort' = list(text = '#ddd', bkg = '#a00', terminal = T, once = T),
  'Note' = list(text = '#ddd', bkg = '#000', terminal = F),
  'Annotate' = list(text = '#ddd', bkg = '#000', terminal = F, once = F)
)

instruments = list(
  'CTD' = c('Deploy', 'Recover', 'Abort'),
  'Bongo' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  'Multinet' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  'Mooring Deployment' = c('Start', 'End', 'Abort'),
  'Mooring Recovery' = c('Start', 'End', 'Abort'),
  'Underway Sample' = c('Collect', 'End', 'Abort'),
  'TM Fish' = c('Start', 'End', 'Abort'),
  'TM CTD' = c('Deploy', 'Recover', 'Abort'),
  'CTD (prod)' = c('Deploy', 'Recover', 'Abort'),
  'Other (see note)' = c('Collect', 'End', 'Abort'),
  'Cruise Note' = c(),
  'Particle Interceptor Trap' = c('Deploy', 'Recover', 'Abort'),
  'ROTV' = c('Deploy', 'Annotate', 'Recover', 'Abort')
)
instruments = instruments[order(names(instruments))]

authors = c(
  '',
  'Thomas Kelly <tbkelly@alaska.edu>',
  'Person2 <other@email.com>',
  'Pet Shrimp <pelagiclyfe@gmail.com>'
)

authors = authors[order(authors)]


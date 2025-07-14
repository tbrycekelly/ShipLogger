## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.
# Computer should be in UTC, because that is the only responsible thing to do.

settings = list(
  datetime.format = '%Y-%m-%d  %H:%M:%S %Z',
  timeouts = list(
    logRefresh = 2, #sec
    pageRefresh = 600, #sec
    positionUpdate = 2, #sec
    uiRefresh = 1, #sec
    mapRefresh = 60 #sc
  ),
  counters = list(
    positionCounter = 10
  )
)


color = list(
  'Deploy' = list(text = '#ddd', bkg = '#224422'),
  'Recover' = list(text = '#ddd', bkg = '#000'),
  'AtDepth' = list(text = '#ddd', bkg = '#000'),
  'Start' = list(text = '#ddd', bkg = '#000'),
  'End' = list(text = '#ddd', bkg = '#000'),
  'Collect' = list(text = '#ddd', bkg = '#000')
)

instruments = list(
  'CTD' = c('Deploy', 'Recover'),
  'Bongo' = c('Deploy', 'AtDepth', 'Recover'),
  'Multinet' = c('Deploy', 'AtDepth', 'Recover'),
  'Mooring Deployment' = c('Start', 'End'),
  'Mooring Recovery' = c('Start', 'End'),
  'Underway Sample' = c('Collect'),
  'TM Fish' = c('Start', 'End'),
  'TM CTD' = c('Deploy', 'Recover'),
  'CTD (prod)' = c('Deploy', 'Recover'),
  'Other (see note)' = c('Collect')
)
instruments = instruments[order(names(instruments))]

authors = c(
  '',
  'Thomas Kelly <tbkelly@alaska.edu>',
  'Person2 <other@email.com>',
  'Pet Shrimp <pelagiclyfe@gmail.com>'
)

authors = authors[order(authors)]


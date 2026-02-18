## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.
# Computer should be in UTC, because that is the only responsible thing to do.

settings = list(
  datetime.format = '%Y-%m-%d  %H:%M:%S %Z',
  timeouts = list(
    logRefresh = 2, #sec
    pageRefresh = 60 * 60, #sec
    positionUpdate = 10, #sec
    uiRefresh = 1, #sec
    mapRefresh = 60 #sc
  ),
  counters = list(
    positionCounter = 10
  ),
  drawIsobath = F,
  databaseFile = 'log/events.db'
)


buttons = list(
  'Deploy' = list(text = '#ddd', bkg = '#224422', terminal = F, once = T),
  'Recover' = list(text = '#ddd', bkg = '#000', terminal = T, once = T),
  'AtDepth' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'Start' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'End' = list(text = '#ddd', bkg = '#000', terminal = T, once = T),
  'Collect' = list(text = '#ddd', bkg = '#000', terminal = T, once = T),
  'Collect ' = list(text = '#ddd', bkg = '#000', terminal = F, once = T),
  'Abort' = list(text = '#ddd', bkg = '#a00', terminal = T, once = T),
  'Cancel' = list(text = '#ddd', bkg = '#a00', terminal = T, once = T),
  'Note' = list(text = '#ddd', bkg = '#000', terminal = F),
  'Annotate' = list(text = '#ddd', bkg = '#000', terminal = F, once = F),
  'Started' = list(text = '#ddd', bkg = '#224422', terminal = F, once = F),
  'Stopped' = list(text = '#ddd', bkg = '#a00', terminal = F, once = F),
  'Maintenance' = list(text = '#ddd', bkg = '#000', terminal = F, once = F)

)


instruments = list(
  'CTD Rosette' = c('Deploy', 'Recover', 'Abort'),
  'Bongo' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  #'Over-the-side Pump' = c('Deploy', 'Recover', 'Abort'),
  #'Multicore' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  'Multinet (tow)' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  'Multinet (vertical)' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),
  'Underway Sample' = c('Collect', 'Cancel'),
  #'Sea Ice Ops' = c('Start', 'End', 'Abort'),
  #'Mooring Deployment' = c('Start', 'End', 'Abort'),
  #'Mooring Recovery' = c('Start', 'End', 'Abort'),
  'Other (see note)' = c('Collect', 'Cancel'),
  'Underway Seawater System' = c('Started', 'Stopped', 'Maintenance'),
  'Cruise Note' = c()

  #'Underway Sample Series' = c('Collect ', 'End', 'Abort'),

  #'Multinet' = c('Deploy', 'AtDepth', 'Recover', 'Abort'),

  #'TM Fish' = c('Start', 'End', 'Abort'),
  #'TM CTD' = c('Deploy', 'Recover', 'Abort'),
  #'CTD (prod)' = c('Deploy', 'Recover', 'Abort'),
  #'Particle Interceptor Trap' = c('Deploy', 'Recover', 'Abort'),
  #'ROTV' = c('Deploy', 'Annotate', 'Recover', 'Abort')
)
instruments = instruments[order(names(instruments))]

authors = c(
  '',
  'Thomas Kelly <tbkelly@alaska.edu>',
  'Mette Kaufman <mrkaufman@alaska.edu>',
'Nicole Webster <nmwebster@alaska.edu>',
'Dan Cushing <dacushing@alaska.edu>',
'David Piper <dbpiper@alaska.edu>',
'Kyle Dilliplaine <kbdilliplaine@alaska.edu>',
'Ruth Varner <rvarner4@alaska.edu>',
'Russ Hopcroft <rrhopcroft@alaska.edu>',
'Alex Poje <apoje@alaska.edu>',
'Nikki Oniu <nikkioniu@gmail.com>',
'Megan Smith <smithmeg@hawaii.edu>',
'Sarah Belcher <sbelcher2@alaska.edu>',
'Natalie Ice <naice@alaska.edu>',
  'Jana Wheat <jana@alutiiqprideak.org>'
)

authors = authors[order(authors)]


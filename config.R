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
  event.ids = c(0:1000),
  sample.ids = c(10050:18000)
)


instruments = c('Aerosol Sampler (Buck)',
                'Aerosol Sampler (Gao)',
                'Beryllium-7',
                'GEOTRACES Carousel',
                'Sea Ice',
                'McLane Pump',
                'Mono-Corer',
                'Multi-Corer',
                'ODF Rosette (Reg Cast)',
                'Other: include note',
                'ODF Rosette (Pigment/Ra/234Th Cast)',
                'Rain Sampler',
                'Surface Ra Pump',
                'Surface TM (Small Boat)',
                'Surface TM (Fish)',
                'Underway System')
instruments = instruments[order(instruments)]

authors = c(
  '',
  'Person1',
  'Person2',
  'Person3'
)

authors = authors[order(authors)]


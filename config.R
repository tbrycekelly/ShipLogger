## Configuration file
# The file config.R is automatically loaded whenever a user accesses ShipLogger so any edits can be
# applied with a simple refresh of the app page.


settings = list(
  update.frequency = 5, #sec
  nmea.address = "0.0.0.0:1234",
  final.action = 'Recover'
)


## instrument List
# A list of instruments available in the drop down menu
instruments = c(
  '',
  'Bongo Net',
  "Calvet Net",
  'CTD',
  'DPI',
  'Iron Fish',
  'Methot',
  'MultiNet',
  'Sediment Trap',
  'TM CTD',
  'Mooring'
)
instruments = instruments[order(instruments)]


# The order of actions is important. The selection will auto increment from the first one until
# the final.action is logged (as set above in settings). When this happens all user input will
# be cleared.
actions = c(
  'Deploy',
  "Recover",
  'Abort'
)


authors = c(
  '',
  'Caitlin Smoot <casmoot@alaska.edu>',
  'Ana Aguilar-Islas <amaguilarislas@alaska.edu>',
  'Sierra Lloyd <sierravlloyd@gmail.com>',
  'Gwenn Hennon <gmhennon@alaska.edu>',
  'Megan Brauner <mbrauner@alaska.edu>',
  'Dan Cushing <dan.cushing@gmail.com>',
  'Suzanne Strom <stroms@wwu.edu>',
  'Kerri Fredrickson <frederk@wwu.edu>',
  'willi359@wwu.edu',
  'Jaime Blais <blaisj@wwu.edu>',
  'Hannah Kepner <hekepner@alaska.edu>',
  'Cara Roberts <cjkoutchak@alaska.edu>',
  'Alexia Wolff <Alexia.g.b.wolff@gmail.com>',
  'Thomas Kelly <tbkelly@alaska.edu>',
  'Xavier Warren <warren.xavierj@gmail.com>',
  'Nicole Webster <nmwebster@alaska.edu>',
  'troutjac@grinnell.edu',
  'Abigail VanPelt <Abigail.Lee.VanPelt@live.mercer.edu>',
  'ballantk@oregonstate.edu',
  'ajmarvy@reed.edu',
  'Addie Norgaard <anorgaard2@alaska.edu>',
  'Russell Hopcroft <rrhopcroft@alaska.edu>',
  'Hana Busse <hana.busse@gmail.com>',
  'Ryan Owens <rpowens@alaska.edu>'
)

authors = authors[order(authors)]









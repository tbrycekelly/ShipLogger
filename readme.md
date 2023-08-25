# ShipLogger

### Getting Started

ShipLogger requires the following R packages to be installed:

    library(shiny)
    library(shinyWidgets)
    library(jsonlite)
    library(data.table)
    library(DT)
    library(serial)
  
Once these dependancies are met, then starting the log is as easy as running the app. I recommend openning the project in RStudio and then running `app.R`.

    source('config.R')
    source('functions.R')
    shiny::runApp(host = '0.0.0.0', port = 80)

But before you do that, you should take a look at the `config.R` file where all the preferences and cruise details are located. The `config.R` file is broadly organized into three parts: (1) settings, (2) instruments and actions, and (3) cruise participants. Starting with the _settings_ list, here is an example:

    settings = list(
      cruise = 'SKQ202307S',
      nmea.type = 'serial', # Options: 'serial' or 'tcp'
      nmea.tcp.host = "0.0.0.0",
      nmea.tcp.port = 1000,
      nmea.serial.port = "com5",
      nmea.serial.mode = "9600,n,8,1",
      nmea.update = 10, #sec
      final.action = 'Recover',
      local.timezone = -8
    )

For initial setup, the appropriate cruise name should be provided. For position information, specify if you are using a _serial_ or _tcp_ connection with your GPS repreater. Next provide the respective configuration information for either the serial or tcp connection. The default `nmea.update` interval of 10 seconds is likely appropriate. The `final.action` name specified at which point the log should reset rather than increment actions. For example, recovery of the CTD usually (and hopefully) follows the deployment of the CTD, so after a CTD deployment event, the appropriate metadata in the form is preserved for the recovery event. Finally, the system time (hopefully UTC!) to local time offset can be given (-8 is UTC-8). 

The next section of the configuration is for setting up the instruments and the actions associated with them. To edit or add an instrument or action, simply add a new list entry or modify the list of actions associated with it. See the __Actions__ section below.

The final configuration task is setting up the author list. This can be easily pulled from a cruise roster or email chain. This format has worked well and provides some valuable information for future log viewers, but set of entries will work.

    authors = c(
      '',
      'Jane Doe <jdoe@email.com>',
      'Peter Jacks <pete@org.edu>',
      'Bosun <bosunbrews@dogs.com>'
    )

By default the list of automatically sorted by alphabetical order, but this can be turned off.

#### Actions

Custom instruments, and the actions associated with them, are easily added in `config.R`. A typical entry will follow this scheme:

    instruments[[<instrument name>]] = c('<action1>', '<action2>', ... '<actionX>')
    
The `instrument name` can be any string eith or without spaces, but it must be unique realtive to the other instruments. Similarly, actions can be anything and _can_ be placed in any order. Note, that the order of actions does matter as it sets both the order of actions within the GUI, but also interacts with the `final.action` setting from above. For am example of how the `final.action` setting works, consider the following instrument entry:

    instruments[['CTD']] = c('Deploy', "Recover", 'Abort', 'At Depth')
    
With `final.action = 'Recover'`, when a users logs a deployment, the form will keep the metadata as entered (inc. station, cast, author) and the action will be incremented by one to __Recover__. When a recovery action is logged, the form will be completely reset since that action is the `final.action'. Alternatively, if the entry was:

    instruments[['CTD']] = c('Deploy', 'At Depth', "Recover", 'Abort')

Then the form would allow Deploy, At Depth, and Recover to be performed in order without any significant user input. Therefore, we recommend in general that the action list be ordered as `Deploy` and `Recover` in that order with any required actions in their respective order between those two and any supplemental actions after the `final.action` (as `Abort` is here). Here are some additonal example configures:

    instruments[['CTD']] = c('Deploy', 'Soaking', 'At Depth', "Recover", 'Abort')
    instruments[['Bongo']] = c('Deploy', 'At Depth', "Recover", 'Abort')
    instruments[['Mooring Recovery']] = c('Pinged', 'Released', 'Tagged', 'On Deck', 'Abort')
    

#### Setting a computer name

It is recommended to set the computer name of the computer running ShipLogger is a convenient and descriptive name. On Windows, this cna be found easily by typing name into the start menu. For example, naming the computer _shiplogger_ may make it accessible to other computers on the network as [http://shiplogger/](http://shiplogger/) (or http://shiplogger.local/ for linux and mac). This is contingent on the ship's network configuration and firewall settings.

#### NMEA Setup and Information

This logger is set up to use standard NMEA sentances from a GPS repeater to access real-time ship location information. In particular, the script will parse available GPGGA sentances from either a serial interface (e.g. COM port) or from a networked TCP socket connection. Virtually all science vessels have one or both options available. For TCP connections, `nmea.type` should be set to __'tcp'__ and the specific values for `nmea.tcp.host` and `nmea.tcp.port` should be provided. For serial interfaces, `nmea.type` should be set to __'serial'__ and a com port specified in `nmea.serial.port`. In general, `nmea.serial.mode` wont need to be adjusted except for ships using a baud rate different than 9600. The `nmea.update` variable sets the number of seconds to wait between attempting to update the position data. In general 10 seconds is an appropriate interval. For reference, an appropriate NMEA GPGGA sentence[[ref]](https://docs.novatel.com/OEM7/Content/Logs/GPGGA.htm) is given below:

    $GPGGA,202530.00,5109.0262,N,11401.8407,W,5,40,0.5,1097.36,M,-17.00,M,18,TSTR*61

Data from the NMEA position feed is internally logged to a dedicated file including system time (used throughout ShipLogger), GPS timestamp, longitude, and latitude. This enables retreival of position data for events that were not logged on time or otherwise edited later on. This position file can be retrieved at any time from the dashboard.

To facilitate testing/diagnostics for the NMEA feed, the `testNMEA` function is provided to validate the settings list provided. A working combination of settings should return:

    Testing NMEA settings:
    Retreived 2 NMEA sentences:
    	 $GPGGA,202530.00,5109.0262,N,11401.8407,W,5,40,0.5,1097.36,M,-17.00,M,18,TSTR*61
    	 $GPGGA,202531.00,5109.0562,N,11401.8537,W,5,40,0.5,1097.36,M,-17.00,M,18,TSTF*62


#### Data Integrety

Information in the log is stored as a text file with each entry saved as a JSON sentence for interoperability. Upon each change to the event log, the current version is copied (renamed with the current timestamp) prior to modification. When an entry is modified in post, the original entry is preserved and an appropriately modified entry is appended. The display and export options only provide the last version of each unique ID, and so modification history can be tracked. Additionally, a diagnostic log is kept to facilitate the finding of problems. At any time, the entire ShipLogger folder can be copied and saved. This provides an easy means of backup.

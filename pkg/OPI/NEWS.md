# OPI 1.0
  * experimental, lots of bugs.
  * not really a proper R package

# OPI 1.1
  * Correct R package structure
  * Introduced chooseOpi()
  * Split out Octopus 900 functions into OPIOctopus900 package
  * Contains Tutorial and pdf Manual
  * Contains FT()

# OPI 1.2
  * contains ZEST()
  * Fixed a bug in SimHenson opiPresent()

# OPI 1.2.1
  * Contains simDisplay to allow displaying of stims on a plot 
    in simulation mode.
  * removed nextStim=NULL bug in opiPresent.opiStaticStimulus octopus900.r

# OPI 1.2.1.1
  * as for 1.2.1 but with depends rJava removed from Description

# OPI 1.2.2
  * Contains multi-location ZEST

# OPI 1.3
  * Contains Temporal and Kinetic in OPIPresent

# OPI 1.4
  * allows for differring params to all opiXX() functions (in opiDistributor)
    but as a result, all params MUST be named in calls
  * added simYes and simNo
  * fixed "domain" bug in ZEST.start likelihood parameter
  * added minStimulus and maxStimulus to ZEST.start and internals of ZEST.step
  * added frame to opiPresent return
  * added param to opiInitialize to turn gaze tracking on/off
  * added alias opiInitialise
  * updated docs.
  * Removed .GlobalEnv and created sub Envs for each Sim
  * updated docs for opiSetBackground for Octopus 900
  * deleted opi.procedure.{load|save}
  * moved chooseOpi() into opi.r
  * NOTE - must pass junk params in .jinit() and .jpackage wont work...

# OPI 1.5
  * opiPresent only returns a single frame (array of int)
  * seen returned from opiPresent is limited to 0 or 1
  * added !exist to opiDistributor
  * simHenson uses false pos rate to determine seen if db < 0 
  * changed ZEST.final to return "limit" 
  * fixed verbose=1 bug in zest.r
  * changed respSeq return type from ZEST

# OPI 1.6
 * added SimHensonRT and related man and data
 * fixed ZEST bug 
 * added warnings about non-OPI parameters to opi functions
 * fixed small typos in ZEST man page
 * added FT.start/step/stop/final/final.details

# OPI 1.6.2
 - branch version that uses Tony's special size wheel, not GOLDMANN

# OPI 1.7
 - altered to look for opi.jar for Octopus 900 in EyeSuite dir, not 
   in .Library or libpaths()
   Hence changed chooseOpi and opitInialise
 - added maxStim param to dbTocd and cdTodb
 - added type="X" to simH and simH_RT

# OPI 2.0
 - moved to a client-server model for Octopus 900

# OPI 2.1
 - included Kowa AP 7000 perimeter
 - small bug fix in O900 client
 - moved to a client-server model for Octopus 900 if stim=NULL for opiPresent
 - added fourTwo procedure

# OPI 2.2
 - added more Kowa functionality, fixed underscore problem
 - fixed stim==NULL bug on O900 client
 - added bigWheel=TRUE in opiInitialise for octo900

# OPI 2.3
 - added support for F310 Logitech Controller for responses to static stimuli
 - added buzzer support for O900
 * fixed Kowa kinetic return values

# OPI 2.4
 * added purkinje coordinates to return of opiQueryDevice for Kowa AP7000

# OPI 2.5
 * added imo
 * added different types of buzzer for Octopus 900
 * added 4000/10000 as 0 dB choice for Octopus 900

# OPI 2.6
 * added Compass support
 * fixed sneaky kinetic speed bug

# OPI 2.7
 * Added Kinetic Stimuli to SimHenson
 * Updated Compass protocl to The Helsinki Draft
 * Added MOCS

# OPI 2.8
 * Added opiPresent error check into MOCS
 * Changed return from MOCS to include all opiPresent return fields.
 * Changed ZEST state to include all opiPresent return fields.
 * Fixed two bugs related to gaze feed in O900
 * Added checkFixationOK check to opiStaticStimulus, ZEST and MOCS (untested)
 * Altered mocs to print comma separated lines and location number.
 * Added Daydream device to allow Google Daydream perimetry ```chooseOpi("Daydream")``` (unfinished)
 * Fixed bug where stimulus color was not transmitted to the O900
 * Updated documentation for opiClose re Compass.

# OPI 2.9
 * Removed all individual environments for each perimeter and made one OPIEnv (not backwards compatible)
    * .Octopus900Env$x has become .OPIEnv$O900$x
 * Added gazeFeed to a folder for Octopus 900
 * Added Display as an OPI client.
 * Fixed cdTodb to stop if given <= 0 input
 * Fixed some color="white" examples. (Color is machine specific)

# OPI 2.10
 * Major rewrite of the Display OPI option.
 * Updates to DayDream code. 
 * Added size option to static stimuli for Compass OPI option.
 * New device PhoneHMD added to support Android phones

# OPI 2.11
 * Changed to Apache 2,0 license.
 * Fixed minor bug in ZEST related to minStimulus.
 * Added .OpiEnv$machine_name which is set to chosen OPI machine
 * 2.11.1 Minor bug fix to export .OpiEnv , added QUEST+
 * 2.11.2 Minor bug fix to encode return image for Compass initialise
 * 2.11.3 Added fixedResponse param to ZEST.step


# OPI 3.0.0 (July 2024)
 * Major rewrite of the OPI 2.11.3 package to include
   - New support for the OPI-JOVP server hence screen-based perimeters
   - Changed from GNU to Apache 2.0 license
   - R code for all machines automatically generated from `rgen` in `OPI-JOVP` package
   - Deprecated opiSetBackground, replaced with opiSetup
   - Deprecated Kinetic simulation: SimHenson etc now only work for static stimuli.
   - Other algorithms and simulations stay the same as before
 * Addition of the imoVifa (aka Tempo)
 * New unit tests
 * Gotchas when working across machines
   - Some of the previous commands were documented to return a list containing `err = NULL`
     on success. For commands where this was the only expected item in the return list, 
     like `opiInitialise` on the Octopus900, this meant they returned an empty list.
     That is, `is.null(result$err)` was trivially true as `is.null(result)` was true. 
     But this is not always the case for `opiInitialise` on all machines. 
     So code for use on more than one machine should check opi function return
     `result` success as `is.null(result$err)`, not `is.null(result)` as in the past.
     Algorithms in this package have been updated to reflect this.

## OPI 3.0.1 
  * Eye tracking for ImoVifa/Tempo added.

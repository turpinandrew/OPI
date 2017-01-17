#
# OPI for Compass
# 
# Based on kowaAP7000Client.r.
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: July 2016 (In Padova!)
#
# Copyright 2016 Andrew Turpin
#
# This program is part of the OPI (http://perimetry.org/OPI).
# OPI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

###################################################################
# .CompassEnv$socket is the connection to the Compass
# .CompassEnv$...    a variety of constants, etc
###################################################################
if (!exists(".CompassEnv")) {
    .CompassEnv <- new.env()

    .CompassEnv$ZERO_DB_IN_ASB <- 10000

    .CompassEnv$MAX_DB <- 50  
    .CompassEnv$MIN_DB <- 0  
    .CompassEnv$MIN_X  <- -30
    .CompassEnv$MAX_X  <- 30  
    .CompassEnv$MIN_Y  <- -30
    .CompassEnv$MAX_Y  <- 30  
    .CompassEnv$MIN_RESP_WINDOW  <- 200
    .CompassEnv$MAX_RESP_WINDOW  <- 2000

    .CompassEnv$SEEN     <- 1  
    .CompassEnv$NOT_SEEN <- 0  

    # Utility functions for validating inputs
    .CompassEnv$minCheck <- function(x, limit, txt) {
        if (x < limit) {
	        opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
	    }
    }
    .CompassEnv$maxCheck <- function(x, limit, txt) {
        if (x > limit) {
	        opiClose()
            stop(paste("opiPresent: ", txt, "is too big (maximum ", limit, ")"))
	    }
    }
}

#######################################################################
# INPUT: 
#   ip    = ip address on which server is listening
#   port  = port number on which server is listening
#
# @return NULL if succeed, stop otherwise
#######################################################################
compass.opiInitialize <- function(ip="192.168.1.2", port=44965) {
    cat("Looking for server... ")
    suppressWarnings(tryCatch(    
        v <- socketConnection(host = ip, port,
                      blocking = TRUE, open = "w+b",
                      timeout = 10)
        , error=function(e) { 
            stop(paste(" cannot find a server at", ip, "on port",port))
        }
    ))
    close(v)
    
    print("found server :)")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to Compass at",ip,"on port", port))
    )

    assign("socket", socket, envir = .CompassEnv)
    
    return(NULL)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#
# Return a list of 
#	err  = string message
#	seen = TRUE if seen, FALSE otherwise
#	time = reaction time (milliseconds)
#   pupildX = list of x-coordinates of pupil relative to PRL position during presentation (degrees)
#   pupildY = list of y-coordinates of pupil relative to PRL position during presentation (degrees)
###########################################################################
compass.opiPresent <- function(stim, nextStim=NULL) { UseMethod("compass.opiPresent") }
setGeneric("compass.opiPresent")

compass.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="The NULL stimulus not supported", seen=NA, time=NA, pupilX=NA, pupilY=NA))
    }

    if(!is.null(stim$size)) warning("opiPresent: ignoring stimulus size")
    if(!is.null(stim$color)) warning("opiPresent: ignoring stimulus color")
    if(!is.null(stim$duration)) warning("opiPresent: ignoring stimulus duration")

    .CompassEnv$minCheck(stim$x, .CompassEnv$MIN_X, "Stimulus x")
    .CompassEnv$maxCheck(stim$x, .CompassEnv$MAX_X, "Stimulus x")
    .CompassEnv$minCheck(stim$y, .CompassEnv$MIN_Y, "Stimulus y")
    .CompassEnv$maxCheck(stim$y, .CompassEnv$MAX_Y, "Stimulus y")
    .CompassEnv$minCheck(stim$responseWindow, .CompassEnv$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .CompassEnv$maxCheck(stim$responseWindow, .CompassEnv$MAX_RESP_WINDOW, "Stimulus responseWindow")
    .CompassEnv$minCheck(stim$level, .CompassEnv$ZERO_DB_IN_ASB/pi/.CompassEnv$MIN_DB, "Stimulus level")
    .CompassEnv$maxCheck(stim$level, .CompassEnv$ZERO_DB_IN_ASB/pi/.CompassEnv$MAX_DB, "Stimulus level")

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-SET-RESPONSE-WINDOW"
    msg <- paste(msg, " ", stim$responseWindow)
    writeLines(msg, .CompassEnv$socket)
    res <- readLines(.CompassEnv$socket, n=1)

    if (res == "ERR")
        return(list(err=paste("Cannot set response window to",stim$responseWindow), seen=FALSE, time=0))

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, cdTodb(stim$level, .CompassEnv$ZERO_DB_IN_ASB/pi))

    writeLines(msg, .CompassEnv$socket)
    res <- readLines(.CompassEnv$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] == "ERR")
        return(list(err="Could not present stim", seen=FALSE, time=0))

    return(list(
      err=NULL,
      seen=ifelse(s[1] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time=as.numeric(s[2]), 
      pupildX=as.numeric(s[3]),
      pupildY=as.numeric(s[4])
    ))
}

########################################## 
# Present kinetic stim, return values 
########################################## 
compass.opiPresent.opiKineticStimulus <- function(stim, ...) {
    warning("Compass does not support kinetic stimuli (yet)")
    return(list(err="Compass does not support kinetic stimuli (yet)", seen=FALSE, time=0))
}

###########################################################################
# Not supported on AP 7000
###########################################################################
compass.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("Compass does not support temporal stimuli (yet)")
    return(list(err="Compass does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

###########################################################################
# set background color and/or fixation marker
# color is one of .CompassEnv$BACKGROUND_WHITE or 
#                 .CompassEnv$BACKGROUND_YELLOW
###########################################################################
compass.opiSetBackground <- function(lum=NA, color=NA, fixation=NA) {
    warning("Compass does not support setting backgrounds (yet)")
    return(list(err="Compass does not support setting backgrounds (yet)", seen=FALSE, time=0))
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
compass.opiClose <- function() {
    writeLines("OPI-CLOSE", .CompassEnv$socket)

    res <- readLines(.CompassEnv$socket, n=1)

    close(.CompassEnv$socket)

    if (res == "ERR") {
        warning("Opi Close returned error")
        return("ERR")
    } else {
        return(NULL)
    }
}

###########################################################################
# Lists defined constants
###########################################################################
compass.opiQueryDevice <- function() {
    return(list(default="Nothing to report", isSim=FALSE))
}

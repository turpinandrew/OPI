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
# Modified
# Fri 30 Jun 2017: updated based on initial draft of protcol (The Helsinki Draft)
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
# @return list of 
#       err NULL if succeed, error code otherwise
#       prl c(x,y) of PRL
#       image retinal image as a jpeg in raw bytes
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
    
    cat("found server at",ip,port,":)\n")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to Compass at",ip,"on port", port))
    )

    assign("socket", socket, envir = .CompassEnv)

    msg <- "OPI-OPEN"
    writeLines(msg, socket)
    
    n <- readBin(socket, "integer", size=4)
#print(paste("opiInitialize read: ", n))
    if (n == 0) {
        return(list(err="opiInitialise Error"))
    } else {
        prlx <- readBin(socket, "double", size=4)
        prly <- readBin(socket, "double", size=4)
        im <- readBin(socket, "raw", n=(n-8), size=1)

        return(list(err=NULL, prl=c(prlx, prly), image=im))    
    }
}

###########################################################################
# INPUT: 
#   As per OPI spec
#
# Return a list of 
#    err             : (integer) 0 all clear, >= 1 some error codes (eg cannot track, etc)
#    seen            : 0 for not seen, 1 for seen (button pressed in response window)
#    time            : in ms (integer) (does this include/exclude the 200ms presentation time?) -1 for not seen.
#    time_rec        : time since epoch when command was received at Compass (integer ms)
#    time_pres       : time since epoch that stimulus was presented (integer ms)
#    num_track_events: number of tracking events that occurred during presentation (integer)
#    num_motor_fails : number of times motor could not follow fixation movement during presentation (integer)
#    pupil_diam      : pupil diameter in mm (float)    
#    loc_x           : pixels integer, location in image of presentation
#    loc_y           : pixels integer, location in image of presentation
###########################################################################
compass.opiPresent <- function(stim, nextStim=NULL) { UseMethod("compass.opiPresent") }
setGeneric("compass.opiPresent")

compass.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="The NULL stimulus not supported", seen=NA, time=NA))
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
    lev <- round(cdTodb(stim$level, .CompassEnv$ZERO_DB_IN_ASB/pi),0)
    .CompassEnv$minCheck(lev, .CompassEnv$MIN_DB, "Stimulus level")
    .CompassEnv$maxCheck(lev, .CompassEnv$MAX_DB, "Stimulus level")

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, lev, "3", 200, stim$responseWindow)

    writeLines(msg, .CompassEnv$socket)
    res <- readLines(.CompassEnv$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] > 0)
        return(list(err=s[1], seen=NA, time=NA))

    return(list(
      err             =NULL,
      seen            =ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time            =as.numeric(s[3]), 
      time_rec        =as.numeric(s[4]),
      time_pres       =as.numeric(s[5]),
      num_track_events=as.numeric(s[6]),
      num_motor_fails =as.numeric(s[7]),
      pupil_diam      =as.numeric(s[8]),
      loc_x           =as.numeric(s[9]),
      loc_y           =as.numeric(s[10])
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
# Used to turn tracking on or off or alter fixation.
###########################################################################
compass.opiSetBackground <- function(lum=NA, color=NA, fixation=NA, tracking_on=NA) {
    if (!is.na(lum) || !is.na(color))
        warning("opiSetBackground: Compass does not support setting background color or luminance.")

    if (!is.na(tracking_on)) {
        if (tracking_on) {
            writeLines("OPI-SET-TRACKING 1", .CompassEnv$socket)
            res <- readLines(.CompassEnv$socket, n=1)
            s <- strsplit(res, " ", fixed=TRUE)[[1]]
            if (s[1] != 0) {
                return(list(error=paste("opiSetBackground: failed turn tracking on ", s[1])))
            }
        } else {
            writeLines("OPI-SET-TRACKING 0", .CompassEnv$socket)
            res <- readLines(.CompassEnv$socket, n=1)
            s <- strsplit(res, " ", fixed=TRUE)[[1]]
            if (s[1] != 0) {
                return(list(error=paste("opiSetBackground: failed turn tracking off ", s[1])))
            }
        }
    }

    if (length(fixation) > 1 || !is.na(fixation)) {
        if (length(fixation) != 3) {
            return(list(error="opiSetBackground: fixation parameter must have 3 fields c(x,y,t)"))
        }
        x <- fixation[1]
        y <- fixation[2]
        t <- fixation[3]

        if (!(x %in% c(-15, -9, -3, 0, 3, 7, 15))) {
            return(list(error="opiSetBackground: fixation x must be in c(-15, -9, -3, 0, 3, 7, 15)"))
        }
        if (!(y %in% c(-3, 0, 3))) {
            return(list(error="opiSetBackground: fixation y must be in c(-3, 0, 3)"))
        }
        if (t == 1 && (!(x %in% c(-3, 0, 3)) || y != 0)) {
            return(list(error="opiSetBackground: fixation type 1 can only be at ({-3,0,+3}, 0)"))
        }
        writeLines(paste("OPI-SET-FIXATION",x,y,t), .CompassEnv$socket)
        res <- readLines(.CompassEnv$socket, n=1)
        s <- strsplit(res, " ", fixed=TRUE)[[1]]
        if (s[1] != 0) {
            return(list(error=paste("opiSetBackground: failed to set fixation: ", s[1])))
        }
    }
    
    return(list(error=NULL))
}

###########################################################################
# return list(err=NULL, fixations=matrix of fixations)
#       matrix has one row per fixation
#       col-1 timestamp (ms since epoch) 
#       col-2 x in degrees 
#       col-3 y in degrees 
###########################################################################
compass.opiClose <- function() {
    writeLines("OPI-CLOSE", .CompassEnv$socket)

    num_bytes <- readBin(.CompassEnv$socket, "integer", size=4)

    if (num_bytes == 0) {
        warning("opiClose returned error")
        return(list(err="ERR"))
    }

    num_triples <- num_bytes/12
    fixations <- matrix(NA, ncol=3, nrow=num_triples)
    for(i in 1:num_triples) {
        fixations[i,1] <- readBin(.CompassEnv$socket, "integer", n=1, size=4)
        fixations[i,2:3] <- readBin(.CompassEnv$socket, "double", n=2, size=4)
    }

    close(.CompassEnv$socket)

    return(list(err=NULL, fixations=fixations))
}

###########################################################################
# Lists defined constants
###########################################################################
compass.opiQueryDevice <- function() {
    return(list(default="Nothing to report", isSim=FALSE))
}


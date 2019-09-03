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
# .OpiEnv$Compass$socket is the connection to the Compass
# .OpiEnv$Compass$...    a variety of constants, etc
###################################################################
if (exists(".OpiEnv") && !exists("Compass", where=.OpiEnv)) {
    assign("Compass", new.env(25), envir=.OpiEnv)
    .OpiEnv$Compass$endian <- "big"   # endianess of the compass OS

    .OpiEnv$Compass$ZERO_DB_IN_ASB <- 10000

    .OpiEnv$Compass$MAX_DB <- 50  
    .OpiEnv$Compass$MIN_DB <- 0  
    .OpiEnv$Compass$MIN_X  <- -30
    .OpiEnv$Compass$MAX_X  <- 30  
    .OpiEnv$Compass$MIN_Y  <- -30
    .OpiEnv$Compass$MAX_Y  <- 30  
    .OpiEnv$Compass$MIN_RESP_WINDOW  <- 0    
    .OpiEnv$Compass$MAX_RESP_WINDOW  <- 2680

    .OpiEnv$Compass$SEEN     <- 1  
    .OpiEnv$Compass$NOT_SEEN <- 0  

    # Utility functions for validating inputs
    .OpiEnv$Compass$minCheck <- function(x, limit, txt) {
        if (x < limit) {
	        opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
	    }
    }
    .OpiEnv$Compass$maxCheck <- function(x, limit, txt) {
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

    assign("socket", socket, envir = .OpiEnv$Compass)

    msg <- "OPI-OPEN"
    writeLines(msg, socket)
    
    n <- readBin(socket, "integer", size=4, endian=.OpiEnv$Compass$endian)

    if (length(n) == 0) {    # Compass was not happy with that OPEN, try until it is (AHT: Sep 2018)
        warning('Compass did not like the OPEN command. Suggest closeAllConnections() and try again')
        return(list(err="Bad open", prl=NULL, onh=NULL, image=NULL))    
    } else {
        #print(paste("opiInitialize read: ", n))
        prlx <- readBin(socket, "double", size=4, endian=.OpiEnv$Compass$endian)
        prly <- readBin(socket, "double", size=4, endian=.OpiEnv$Compass$endian)
        onhx <- readBin(socket, "double", size=4, endian=.OpiEnv$Compass$endian)
        onhy <- readBin(socket, "double", size=4, endian=.OpiEnv$Compass$endian)
        im <- readBin(socket, "raw", n=(n-16), size=1, endian=.OpiEnv$Compass$endian)

        return(list(err=NULL, prl=c(prlx, prly), onh=c(onhx, onhy), image=im))    
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

    .OpiEnv$Compass$minCheck(stim$x, .OpiEnv$Compass$MIN_X, "Stimulus x")
    .OpiEnv$Compass$maxCheck(stim$x, .OpiEnv$Compass$MAX_X, "Stimulus x")
    .OpiEnv$Compass$minCheck(stim$y, .OpiEnv$Compass$MIN_Y, "Stimulus y")
    .OpiEnv$Compass$maxCheck(stim$y, .OpiEnv$Compass$MAX_Y, "Stimulus y")
    .OpiEnv$Compass$minCheck(stim$responseWindow, .OpiEnv$Compass$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .OpiEnv$Compass$maxCheck(stim$responseWindow, .OpiEnv$Compass$MAX_RESP_WINDOW, "Stimulus responseWindow")
    lev <- round(cdTodb(stim$level, .OpiEnv$Compass$ZERO_DB_IN_ASB/pi),0)
    .OpiEnv$Compass$minCheck(lev, .OpiEnv$Compass$MIN_DB, "Stimulus level")
    .OpiEnv$Compass$maxCheck(lev, .OpiEnv$Compass$MAX_DB, "Stimulus level")

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, lev, "3", 200, stim$responseWindow)

    presentBad <- TRUE
    presentCount <- 0
    while (presentBad) {
        presentCount <- presentCount + 1
        if (presentCount %% 10 == 0)
            warning(paste('opiPresent: I have tried presenting',presentCount,'times.'))

        writeLines(msg, .OpiEnv$Compass$socket)
        res <- readLines(.OpiEnv$Compass$socket, n=1)
        s <- strsplit(res, " ", fixed=TRUE)[[1]]

        presentBad <- s[1] > 0
    }

    # BUG - num_track_events, etc are for a single call to the protocolo, and not aggregated over the
    # 'presentBad' loop.

    return(list(
      err             =NULL,
      seen            =ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time            =as.numeric(s[3]), 
      time_hw         =as.numeric(s[4]),
      time_rec        =as.numeric(s[5]),
      time_resp       =as.numeric(s[6]),
      num_track_events=as.numeric(s[7]),
      num_motor_fails =as.numeric(s[8]),
      pupil_diam      =as.numeric(s[9]),
      loc_x           =as.numeric(s[10]),
      loc_y           =as.numeric(s[11])
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
            writeLines("OPI-SET-TRACKING 1", .OpiEnv$Compass$socket)
            res <- readLines(.OpiEnv$Compass$socket, n=1)
            s <- strsplit(res, " ", fixed=TRUE)[[1]]
            if (s[1] != 0) {
                return(list(error=paste("opiSetBackground: failed turn tracking on ", s[1])))
            }
        } else {
            writeLines("OPI-SET-TRACKING 0", .OpiEnv$Compass$socket)
            res <- readLines(.OpiEnv$Compass$socket, n=1)
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

        if (!(x %in% c(-20, -6, -3, 0, 3, 6, 20))) {
            return(list(error="opiSetBackground: fixation x must be in c(-20, -6, -3, 0, 3, 6, 20)"))
        }
        if (y != 0) {
            return(list(error="opiSetBackground: fixation y must be 0"))
        }
        if (t == 1 && (!(x %in% c(-3, 0, 3)))) {
            return(list(error="opiSetBackground: fixation type 1 can only be at ({-3,0,+3}, 0)"))
        }
        writeLines(paste("OPI-SET-FIXATION",x,y,t), .OpiEnv$Compass$socket)
        res <- readLines(.OpiEnv$Compass$socket, n=1)
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
    writeLines("OPI-CLOSE", .OpiEnv$Compass$socket)

    num_bytes <- readBin(.OpiEnv$Compass$socket, "integer", size=4, endian=.OpiEnv$Compass$endian)
    print(paste("Num bytes", num_bytes))

    if (num_bytes == 0) {
        warning("opiClose() returned no bytes - perhaps you forgot opiInitialise")
        return(list(err="No Bytes"))
    }

    num_triples <- num_bytes/12
    fixations <- matrix(NA, ncol=3, nrow=num_triples)
    for(i in 1:num_triples) {
        fixations[i,1] <- readBin(.OpiEnv$Compass$socket, "integer", n=1, size=4, endian=.OpiEnv$Compass$endian)
        fixations[i,2:3] <- readBin(.OpiEnv$Compass$socket, "double", n=2, size=4,  endian=.OpiEnv$Compass$endian)
    }

    close(.OpiEnv$Compass$socket)

    return(list(err=NULL, fixations=fixations))
}

###########################################################################
# Lists defined constants
###########################################################################
compass.opiQueryDevice <- function() {
    return(list(default="Nothing to report", isSim=FALSE))
}

#
# OPI for Google Daydream
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: Feb 2019 
#
# Copyright 2019 Andrew Turpin
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
#

###################################################################
# .DayDreamEnv$socket is the connection to the daydream
# .DayDreamEnv$LUT has 256 entries. LUT[x] is cd/m^2 value for grey level x
# .DayDreamEnv$degrees_to_pixels is a funciton from (x,y) in degrees to (x,y) in pixels
# .DayDreamEnv$...    a variety of constants, etc
###################################################################
if (!exists(".DayDreamEnv")) {
    .DayDreamEnv <- new.env()

    .DayDreamEnv$LUT <- NULL
    .DayDreamEnv$degrees_to_pixels <- NULL

    .DayDreamEnv$SEEN     <- 1  
    .DayDreamEnv$NOT_SEEN <- 0  
}

#######################################################################
# INPUT: 
#   ip    = ip address on which server is listening
#   port  = port number on which server is listening
#   lut   = lut[x] is cd/m^2 for grey level x. length(LUT) == 256
#   degrees_to_pixels  = function(x,y) in degress to (x,y) in pixels
#
# @return list of 
#       err = NULL if succeed, will stop otherwise
#######################################################################
daydream.opiInitialize <- function(
        ip="127.0.0.1",
        port=50008, 
        lut = rep(1000, 256),
        degrees_to_pixels=function(x,y) return(20 * c(x,y))
    ) {
    cat("Looking for phone... ")
    suppressWarnings(tryCatch(    
        v <- socketConnection(host = ip, port,
                      blocking = TRUE, open = "w+b",
                      timeout = 10)
        , error=function(e) { 
            stop(paste(" cannot find a phone at", ip, "on port",port))
        }
    ))
    close(v)
    
    cat("found phone at",ip,port,":)\n")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to phone at",ip,"on port", port))
    )

    assign("socket", socket, envir = .DayDreamEnv)
    assign("LUT", lut, envir = .DayDreamEnv)
    assign("degrees_to_pixels", d2p, envir = .DayDreamEnv)

    return(list(err=NULL))
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
daydream.opiPresent <- function(stim, nextStim=NULL) { UseMethod("daydream.opiPresent") }
setGeneric("daydream.opiPresent")

daydream.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) return(list(err="The NULL stimulus not supported", seen=NA, time=NA))

    if (is.null(stim$x) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$y) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$size) return(list(err="No size in stimulus", seen=NA, time=NA))
    if (is.null(stim$level) return(list(err="No level in stimulus", seen=NA, time=NA))
    if (is.null(stim$duration) return(list(err="No duration in stimulus", seen=NA, time=NA))
    if (is.null(stim$responseWindow) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))


################## Up to here....

    xy <- .DayDreamEnv$degrees_to_pixels(stim$x,stim$y)


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

        if (!(x %in% c(-20, -6, -3, 0, 3, 6, 20))) {
            return(list(error="opiSetBackground: fixation x must be in c(-20, -6, -3, 0, 3, 6, 20)"))
        }
        if (y != 0) {
            return(list(error="opiSetBackground: fixation y must be 0"))
        }
        if (t == 1 && (!(x %in% c(-3, 0, 3)))) {
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

    num_bytes <- readBin(.CompassEnv$socket, "integer", size=4, endian=.CompassEnv$endian)
    print(paste("Num bytes", num_bytes))

    if (num_bytes == 0) {
        warning("opiClose() returned no bytes - perhaps you forgot opiInitialise")
        return(list(err="No Bytes"))
    }

    num_triples <- num_bytes/12
    fixations <- matrix(NA, ncol=3, nrow=num_triples)
    for(i in 1:num_triples) {
        fixations[i,1] <- readBin(.CompassEnv$socket, "integer", n=1, size=4, endian=.CompassEnv$endian)
        fixations[i,2:3] <- readBin(.CompassEnv$socket, "double", n=2, size=4,  endian=.CompassEnv$endian)
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


#
# OPI for imo
# 
# Based on imoClient.r.
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: February 2016
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
# .imoEnv$socket is the connection to the imo
# .imoEnv$...    a variety of constants 
###################################################################
if (!exists(".imoEnv")) {
    .imoEnv <- new.env()

    .imoEnv$checkOK <- function(txt, stop_if_bad=TRUE) {
        res <- readBin(.imoEnv$socket, what="integer", size=1, n=1)
        if (res != 0) {
            if (stop_if_bad)
                stop(paste(txt, "did not return OK from imo"))
            else
                warning(paste(txt, "did not return OK from imo"))
        }
    }
}

#######################################################################
# INPUT: 
#   ip                       = ip address on which server is listening
#   port                     = port number on which server is listening
#
# @return NULL if succeed
# @return 1    server not found/ready at the ip+port provided
#######################################################################
imo.opiInitialize <- function(ip= "localhost", port=9999) {
    cat("Looking for server... ")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to IMO at",ip,"on port", port))
    )

    print("found server :)")

    assign("socket", socket, envir = .imoEnv)
    writeChar("STAR", socket, nchars=4, eos=NULL)
    .imoEnv$checkOK("Start", stop_if_bad=TRUE)
    
    return(NULL)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#
# Return a list of 
#	err  = string message
#	seen = TRUE if seen, FALSE otherwise
#	time = reaction time
#   xs = list of x-coordinates of pupil position during presentation
#   ys = list of y-coordinates of pupil position during presentation
###########################################################################
imo.opiPresent <- function(stim, nextStim=NULL) { UseMethod("imo.opiPresent") }
setGeneric("imo.opiPresent")

imo.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, time=NA, pupilX=NA, pupilY=NA))
    }

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    if (is.null(stim$image) || (length(stim$image) != 2))
        warning("opiPresent: expecting a list of 2 matrices in stim$image")

    if (all(dim(stim$image[[1]]) != c(1080,1080))) 
        warning(".imo$createImage: expecting im_list[[1]] to be a 1080x1080 matrix")
    if (all(dim(stim$image[[2]]) != c(1080,1080))) 
        warning(".imo$createImage: expecting im_list[[2]] to be a 1080x1080 matrix")

    writeChar("LOAD", .imoEnv$socket, nchars=4, eos=NULL)
    for (i in 1:nrow(stim$image[[1]]))
        writeBin(as.integer(stim$image[[1]][i,]), .imoEnv$socket, size=1)
    for (i in 1:nrow(stim$image[[2]]))
        writeBin(as.integer(stim$image[[2]][i,]), .imoEnv$socket, size=1)

    .imoEnv$checkOK("LOAD", stop_if_bad=TRUE)

    writeChar("PRES", .imoEnv$socket, nchars=4, eos=NULL)
    writeBin(as.integer(1), .imoEnv$socket, size=4, endian="big")

    writeBin(as.integer(0),   .imoEnv$socket, size=4, endian="big")  # image number 0
    writeBin(as.integer(2),   .imoEnv$socket, size=4, endian="big")  # both eyes
    writeBin(as.integer(stim$duration), .imoEnv$socket, size=4, endian="big")  # pres time

    writeBin(as.integer(0),                   .imoEnv$socket, size=4, endian="big")  # cycle
    writeBin(as.integer(stim$responseWindow), .imoEnv$socket, size=4, endian="big")  # wait

    if (any(names(stim) == "tracking")) 
        writeBin(as.integer(stim$tracking), .imoEnv$socket, size=4, endian="big")  # tracking
    else
        writeBin(as.integer(1), .imoEnv$socket, size=4, endian="big")  # tracking

    .imoEnv$checkOK("PRES", stop_if_bad=TRUE)

    p    <- readBin(.imoEnv$socket, what="integer", size=4, n=1)
    time <- readBin(.imoEnv$socket, what="integer", size=4, n=1)
    n    <- readBin(.imoEnv$socket, what="integer", size=4, n=1)

    pupil <- ellipse <- times <- NULL
    for (i in 1:n) {
        d <- readBin(.imoEnv$socket, what="double", size=4, n=7)
        if (d[1] == 1) {
            pupil   <- c(pupil  , list(list(dx=d[2], dy=d[3])))
            ellipse <- c(ellipse, list(list(d1=d[4], d2=d[5], rho=d[6])))
            times   <- c(times  , d[7])
        } else {
            pupil   <- c(pupil  , list(list(dx=NA, dy=NA)))
            ellipse <- c(ellipse, list(list(d1=NA, d2=NA, rho=NA)))
            times   <- c(times  , NA)
        }
    }

    return(list(
      err=NULL,
      seen=ifelse(p == 1, TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time=time,
      pupilDxDy=pupil, 
      pupilEllipse=ellipse, 
      pupilTimes=times
    ))
}

########################################## 
# Present kinetic stim, return values 
########################################## 
imo.opiPresent.opiKineticStimulus <- function(stim, ...) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, x=NA, y=NA))
    }

    if (length(xy.coords(stim$path)$x) > 2) 
        warning("opiPresent (kinetic): Kowa AP-7000 only supports paths of length 2 (start and end).  Ignoring all but the first two elements of stim$path etc")

        # convert sizes to .imoEnv$SIZES_DEGREES
    stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.imoEnv$SIZES_DEGREES - s))
         if(abs(.imoEnv$SIZES_DEGREES[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         } 
         return(i)
    })

    if (!is.element(stim$colors[1], c(.imoEnv$COLOR_WHITE,
                                  .imoEnv$COLOR_GREEN,
                                  .imoEnv$COLOR_BLUE ,
                                  .imoEnv$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
     }

    .imoEnv$minCheck(xy.coords(stim$path)$x[1], -80, "Start x")
    .imoEnv$maxCheck(xy.coords(stim$path)$x[1], 80, "Start x")
    .imoEnv$minCheck(xy.coords(stim$path)$x[2], -80, "End x")
    .imoEnv$maxCheck(xy.coords(stim$path)$x[2], 80, "End x")
    .imoEnv$minCheck(xy.coords(stim$path)$y[1], -70, "Start y")
    .imoEnv$maxCheck(xy.coords(stim$path)$y[1], 65, "Start y")
    .imoEnv$minCheck(xy.coords(stim$path)$y[2], -70, "End y")
    .imoEnv$maxCheck(xy.coords(stim$path)$y[2], 65, "End y")
    .imoEnv$minCheck(stim$levels[1],  10000/pi/10^5, "Stimulus level")
    .imoEnv$maxCheck(stim$levels[1],  10000/pi     , "Stimulus level")
    .imoEnv$minCheck(stim$speeds[1],  3, "Stimulus speed")
    .imoEnv$maxCheck(stim$speeds[1],  5, "Stimulus speed")

    msg <- "OPI-PRESENT-KINETIC "
    xs <- xy.coords(stim$path)$x[1]
    ys <- xy.coords(stim$path)$y[1]
    msg <- paste(msg, xy.coords(stim$path)$x[1])
    msg <- paste(msg, xy.coords(stim$path)$y[1])
    msg <- paste(msg, xy.coords(stim$path)$x[2])
    msg <- paste(msg, xy.coords(stim$path)$y[2])
    msg <- paste(msg, cdTodb(stim$levels[1], maxStim=10000/pi))
    msg <- paste(msg, stim$sizes[1])
    msg <- paste(msg, stim$colors[1])
    msg <- paste(msg, stim$speeds[1])
	msg <- paste(msg, "\r", sep="")
   
    writeLines(msg, .imoEnv$socket)
    res <- readLines(.imoEnv$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiPresent Kinetic failed")

    return(list(
        err =NULL, 
        seen=ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
        time=NA,
        x=as.numeric(s[3]),     # in degrees
        y=as.numeric(s[4])       # in degrees
    ))
}

###########################################################################
# Not supported on AP 7000
###########################################################################
imo.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    opiClose()
    stop("opiPresent: Kowa AP 7000 does not support temporal stimuli")
}#opiPresent.opiTemporalStimulus()

###########################################################################
# set background color and/or fixation marker
# color is one of .imoEnv$BACKGROUND_WHITE or 
#                 .imoEnv$BACKGROUND_YELLOW
###########################################################################
imo.opiSetBackground <- function(lum=NA, color=NA, fixation=NA) {
    if (!is.na(fixation)) {
        .imoEnv$minCheck(fixation, 0, "Fixation")
        .imoEnv$maxCheck(fixation, 3, "Fixation")

        msg <- paste("OPI-SET-FIXATION ", fixation, "\r", sep="")
        writeLines(msg, .imoEnv$socket)
        .imoEnv$checkOK("opiSetBackground fixation")
    }

    if (!is.na(lum) && !is.na(color)) {
        if (lum == 10 && color != .imoEnv$BACKGROUND_WHITE)
            warning("Can only have a 10 cd/m^2 background that is white")
        if (lum == 100 && color != .imoEnv$BACKGROUND_YELLOW)
            warning("Can only have a 100 cd/m^2 background that is yellow")
    }

    if (!is.na(lum) && is.na(color)) {
        if (lum == 10) {
            color <- .imoEnv$BACKGROUND_WHITE
            warning("Can only have a 10 cd/m^2 background that is white")
        } else if (lum == 100) {
            color <- .imoEnv$BACKGROUND_YELLOW
            warning("Can only have a 100 cd/m^2 background that is yellow")
        } else {
            opiClose()
            stop("opiSetBackground: Can only have 10 cd/m^2 (white) or 100 cd/m^2 (yellow)")
        }
    }
    
    if (!is.na(color)) {
        .imoEnv$minCheck(color, 0, "Background color")
        .imoEnv$maxCheck(color, 1, "Background color")
        msg <- paste0("OPI-SET-BACKGROUND ", color,"\r")
        writeLines(msg, .imoEnv$socket)
        .imoEnv$checkOK("opiSetBackground color")
    }
        
    return(NULL)
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
imo.opiClose <- function() {
    writeChar("STOP", .imoEnv$socket, nchars=4, eos=NULL)
    .imoEnv$checkOK("opiClose")
    close(.imoEnv$socket)
    return(NULL)
}

###########################################################################
# Lists defined constants
###########################################################################
imo.opiQueryDevice <- function() {
    cat("Defined constants and functions\n")
    cat("-------------------------------\n")
    ls(envir=.imoEnv)

    writeLines("OPI-GET-PUPILPOS\r", .imoEnv$socket)
    res <- readLines(.imoEnv$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiQueryDevice failed")

    return(list(
        pupilX=strtoi(s[2]), 
        pupilY=strtoi(s[3]),       # in pixels
        purkinjeX=strtoi(s[4]), 
        purkinjeY=strtoi(s[5])       # in pixels
    ))
}

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
# .OpiEnv$Imo$socket is the connection to the imo
# .OpiEnv$Imo$...    a variety of constants 
###################################################################
if (exists(".OpiEnv") && !exists("Imo", where=.OpiEnv)) {
    assign("Imo", new.env(25), envir=.OpiEnv)

    .OpiEnv$Imo$checkOK <- function(txt, stop_if_bad=TRUE) {
        res <- readBin(.OpiEnv$Imo$socket, what="integer", size=1, n=1)
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
#' @rdname opiInitialize
#' @details
#' \subsection{imo}{
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{imo}, then you must specify the IP
#'   address and port of the imo server.
#'   
#'   \itemize{
#'     \item \code{ip} is the IP address of the imo server as a string.
#'     \item \code{port} is the TCP/IP port of the imo server as a number.
#'   }
#' }
#' @return
#' \subsection{imo}{
#'   Always returns NULL. Will \code{stop} if there is an error.
#' }
#' @examples
#' \dontrun{
#'   # Set up the imo
#'   chooseOpi("imo")
#'   opiInitialize(ip="192.168.1.7", port=44965)
#' }
imo.opiInitialize <- function(ip= "localhost", port=9999) {
    cat("Looking for server... ")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to IMO at",ip,"on port", port))
    )

    print("found server :)")

    assign("socket", socket, envir = .OpiEnv$Imo)
    writeChar("STAR", socket, nchars=4, eos=NULL)
    .OpiEnv$Imo$checkOK("Start", stop_if_bad=TRUE)
    
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
#' @rdname opiPresent
#' @details
#' \subsection{imo}{
#'   DETAILS HERE
#' }
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

    writeChar("LOAD", .OpiEnv$Imo$socket, nchars=4, eos=NULL)
    for (i in 1:nrow(stim$image[[1]]))
        writeBin(as.integer(stim$image[[1]][i,]), .OpiEnv$Imo$socket, size=1)
    for (i in 1:nrow(stim$image[[2]]))
        writeBin(as.integer(stim$image[[2]][i,]), .OpiEnv$Imo$socket, size=1)

    .OpiEnv$Imo$checkOK("LOAD", stop_if_bad=TRUE)

    writeChar("PRES", .OpiEnv$Imo$socket, nchars=4, eos=NULL)
    writeBin(as.integer(1), .OpiEnv$Imo$socket, size=4, endian="big")

    writeBin(as.integer(0),   .OpiEnv$Imo$socket, size=4, endian="big")  # image number 0
    writeBin(as.integer(2),   .OpiEnv$Imo$socket, size=4, endian="big")  # both eyes
    writeBin(as.integer(stim$duration), .OpiEnv$Imo$socket, size=4, endian="big")  # pres time

    writeBin(as.integer(0),                   .OpiEnv$Imo$socket, size=4, endian="big")  # cycle
    writeBin(as.integer(stim$responseWindow), .OpiEnv$Imo$socket, size=4, endian="big")  # wait

    if (any(names(stim) == "tracking")) 
        writeBin(as.integer(stim$tracking), .OpiEnv$Imo$socket, size=4, endian="big")  # tracking
    else
        writeBin(as.integer(1), .OpiEnv$Imo$socket, size=4, endian="big")  # tracking

    .OpiEnv$Imo$checkOK("PRES", stop_if_bad=TRUE)

    p    <- readBin(.OpiEnv$Imo$socket, what="integer", size=4, n=1)
    time <- readBin(.OpiEnv$Imo$socket, what="integer", size=4, n=1)
    n    <- readBin(.OpiEnv$Imo$socket, what="integer", size=4, n=1)

    pupil <- ellipse <- times <- NULL
    for (i in 1:n) {
        d <- readBin(.OpiEnv$Imo$socket, what="double", size=4, n=7)
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

        # convert sizes to .OpiEnv$Imo$SIZES_DEGREES
    stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.OpiEnv$Imo$SIZES_DEGREES - s))
         if(abs(.OpiEnv$Imo$SIZES_DEGREES[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         } 
         return(i)
    })

    if (!is.element(stim$colors[1], c(.OpiEnv$Imo$COLOR_WHITE,
                                  .OpiEnv$Imo$COLOR_GREEN,
                                  .OpiEnv$Imo$COLOR_BLUE ,
                                  .OpiEnv$Imo$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
     }

    .OpiEnv$Imo$minCheck(xy.coords(stim$path)$x[1], -80, "Start x")
    .OpiEnv$Imo$maxCheck(xy.coords(stim$path)$x[1], 80, "Start x")
    .OpiEnv$Imo$minCheck(xy.coords(stim$path)$x[2], -80, "End x")
    .OpiEnv$Imo$maxCheck(xy.coords(stim$path)$x[2], 80, "End x")
    .OpiEnv$Imo$minCheck(xy.coords(stim$path)$y[1], -70, "Start y")
    .OpiEnv$Imo$maxCheck(xy.coords(stim$path)$y[1], 65, "Start y")
    .OpiEnv$Imo$minCheck(xy.coords(stim$path)$y[2], -70, "End y")
    .OpiEnv$Imo$maxCheck(xy.coords(stim$path)$y[2], 65, "End y")
    .OpiEnv$Imo$minCheck(stim$levels[1],  10000/pi/10^5, "Stimulus level")
    .OpiEnv$Imo$maxCheck(stim$levels[1],  10000/pi     , "Stimulus level")
    .OpiEnv$Imo$minCheck(stim$speeds[1],  3, "Stimulus speed")
    .OpiEnv$Imo$maxCheck(stim$speeds[1],  5, "Stimulus speed")

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
   
    writeLines(msg, .OpiEnv$Imo$socket)
    res <- readLines(.OpiEnv$Imo$socket, n=1)
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
# color is one of .OpiEnv$Imo$BACKGROUND_WHITE or 
#                 .OpiEnv$Imo$BACKGROUND_YELLOW
###########################################################################
#' @rdname opiSetBackground
#' @details
#' \subsection{imo}{
#'   DETAILS
#' }
#' @return
#' \subsection{imo}{ 
#'   DETAILS
#' }
imo.opiSetBackground <- function(lum=NA, color=NA, fixation=NA) {
    if (!is.na(fixation)) {
        .OpiEnv$Imo$minCheck(fixation, 0, "Fixation")
        .OpiEnv$Imo$maxCheck(fixation, 3, "Fixation")

        msg <- paste("OPI-SET-FIXATION ", fixation, "\r", sep="")
        writeLines(msg, .OpiEnv$Imo$socket)
        .OpiEnv$Imo$checkOK("opiSetBackground fixation")
    }

    if (!is.na(lum) && !is.na(color)) {
        if (lum == 10 && color != .OpiEnv$Imo$BACKGROUND_WHITE)
            warning("Can only have a 10 cd/m^2 background that is white")
        if (lum == 100 && color != .OpiEnv$Imo$BACKGROUND_YELLOW)
            warning("Can only have a 100 cd/m^2 background that is yellow")
    }

    if (!is.na(lum) && is.na(color)) {
        if (lum == 10) {
            color <- .OpiEnv$Imo$BACKGROUND_WHITE
            warning("Can only have a 10 cd/m^2 background that is white")
        } else if (lum == 100) {
            color <- .OpiEnv$Imo$BACKGROUND_YELLOW
            warning("Can only have a 100 cd/m^2 background that is yellow")
        } else {
            opiClose()
            stop("opiSetBackground: Can only have 10 cd/m^2 (white) or 100 cd/m^2 (yellow)")
        }
    }
    
    if (!is.na(color)) {
        .OpiEnv$Imo$minCheck(color, 0, "Background color")
        .OpiEnv$Imo$maxCheck(color, 1, "Background color")
        msg <- paste0("OPI-SET-BACKGROUND ", color,"\r")
        writeLines(msg, .OpiEnv$Imo$socket)
        .OpiEnv$Imo$checkOK("opiSetBackground color")
    }
        
    return(NULL)
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
#' @rdname opiClose
#' @return
#' \subsection{imo}{
#'   DETAILS
#' }
imo.opiClose <- function() {
    writeChar("STOP", .OpiEnv$Imo$socket, nchars=4, eos=NULL)
    .OpiEnv$Imo$checkOK("opiClose")
    close(.OpiEnv$Imo$socket)
    return(NULL)
}

###########################################################################
# Lists defined constants
###########################################################################
#' @rdname opiQueryDevice
#' @title Query device using OPI
#' \subsection{imo}{
#'   DETAILS
#' }
#' \subsection{imo}{
#'   DETAILS
#' }
imo.opiQueryDevice <- function() {
    cat("Defined constants and functions\n")
    cat("-------------------------------\n")
    ls(envir=.OpiEnv$Imo)

    writeLines("OPI-GET-PUPILPOS\r", .OpiEnv$Imo$socket)
    res <- readLines(.OpiEnv$Imo$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiQueryDevice failed")

    return(list(
        isSim=FALSE,
        pupilX=strtoi(s[2]), 
        pupilY=strtoi(s[3]),       # in pixels
        purkinjeX=strtoi(s[4]), 
        purkinjeY=strtoi(s[5])       # in pixels
    ))
}

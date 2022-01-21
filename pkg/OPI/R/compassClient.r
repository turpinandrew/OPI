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
    .OpiEnv$Compass$MIN_DURATION  <- 1

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
#' @rdname opiInitialize
#' @param ip ip address on which server is listening
#' @param port port number on which server is listening
#' @details
#' \subsection{Compass}{
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{Compass}, then you must specify
#'   the IP address and port of the Compass server.
#'   
#'   \itemize{
#'     \item\code{ip} is the IP address of the Compass server as a string.
#'     \item\code{port} is the TCP/IP port of the Compass server as a number.
#'   }
#'   Warning: this returns a list, not a single error code.
#' }
#' @return
#' \subsection{Compass}{
#'   Returns a list with elements:
#'   \itemize{
#'     \item{err} NULL if successful, not otherwise.
#'     \item{prl} a pair giving the (x,y) in degrees of the Preferred Retinal
#'       Locus detected in the initial alignment.
#'     \item{onh} a pair giving the (x,y) in degrees of the ONH as selected by
#'     the user.
#'     \item{image} raw bytes being the JPEG compressed infra-red image acquired
#'     during alignment.
#'   }
#' }
#' @examples
#' \dontrun{
#'   # Set up the Compass
#'   chooseOpi("Compass")
#'   result <- opiInitialize(ip="192.168.1.7", port=44965)
#'   if (is.null(result$err))
#'     print(result$prl)
#' }
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
###########################################################################
#' @rdname opiPresent
#' @param stim a list of class \code{\link{opiStaticStimulus}},
#'             \code{\link{opiKineticStimulus}}, or \code{\link{opiTemporalStimulus}} to be presented.
#' @param nextStim unused - included for compliance with OPI standard.
#' @details
#' \subsection{Compass}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   If the chosen OPI implementation is \code{Compass}, then \code{nextStim}
#'   is ignored. Note that the dB level is rounded to the nearest integer.
#'   
#'   If tracking is on, then this will block until the tracking is obtained,
#'   and the stimulus presented.
#' }
#' @return
#' \subsection{Compass}{
#'  A list containing
#'  \itemize{
#'    \item{err}{0 all clear, >= 1 some error codes (eg cannot track, etc) (integer)}
#'    \item{seen}{\code{FALSE} for not seen, \code{TRUE} for seen (button pressed in response window)}
#'    \item{time}{response time in ms (integer) since stimulus onset, -1 for not seen}
#'    \item{time_rec}{time since epoch when command was received at Compass (integer ms)}
#'    \item{time_pres}{time since epoch that stimulus was presented (integer ms)}
#'    \item{num_track_events}{number of tracking events that occurred during presentation (integer)}
#'    \item{num_motor_fails}{number of times motor could not follow fixation movement during presentation (integer)}
#'    \item{pupil_diam}{pupil diameter in mm (float)}
#'    \item{loc_x}{pixels integer, location in image of presentation (integer)}
#'    \item{loc_y}{pixels integer, location in image of presentation (integer)}
#'  }
#' }
compass.opiPresent <- function(stim, nextStim=NULL) { UseMethod("compass.opiPresent") }
setGeneric("compass.opiPresent")

compass.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="The NULL stimulus not supported", seen=NA, time=NA))
    }

    if(!is.null(stim$size)) warning("opiPresent: ignoring stimulus size")
    if(!is.null(stim$color)) warning("opiPresent: ignoring stimulus color")

    .OpiEnv$Compass$minCheck(stim$x, .OpiEnv$Compass$MIN_X, "Stimulus x")
    .OpiEnv$Compass$maxCheck(stim$x, .OpiEnv$Compass$MAX_X, "Stimulus x")
    .OpiEnv$Compass$minCheck(stim$y, .OpiEnv$Compass$MIN_Y, "Stimulus y")
    .OpiEnv$Compass$maxCheck(stim$y, .OpiEnv$Compass$MAX_Y, "Stimulus y")
    .OpiEnv$Compass$minCheck(stim$duration, .OpiEnv$Compass$MIN_DURATION, "Stimulus duration")
    .OpiEnv$Compass$minCheck(stim$responseWindow, .OpiEnv$Compass$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .OpiEnv$Compass$maxCheck(stim$responseWindow, .OpiEnv$Compass$MAX_RESP_WINDOW, "Stimulus responseWindow")
    lev <- round(cdTodb(stim$level, .OpiEnv$Compass$ZERO_DB_IN_ASB/pi),0)
    .OpiEnv$Compass$minCheck(lev, .OpiEnv$Compass$MIN_DB, "Stimulus level")
    .OpiEnv$Compass$maxCheck(lev, .OpiEnv$Compass$MAX_DB, "Stimulus level")

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, lev, "3", stim$duration, stim$responseWindow)

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
# Not supported on Compass
###########################################################################
compass.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("Compass does not support temporal stimuli (yet)")
    return(list(err="Compass does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

#' @rdname opiSetBackground
#' @param tracking_on \code{TRUE} for tracking on, \code{FALSE} for off
#' @details
#' \subsection{Compass}{
#'   \code{opiSetBackground(fixation=NA, tracking_on=NA)}
#'   \itemize{
#'     \item{\code{fixation}=c(x,y,t)} where
#'     \itemize{
#'       \item{\code{x}} is one of -20, -6, -3, 0, 3, 6, 20 degrees.
#'       \item{\code{y}} is 0 degrees.
#'       \item{\code{t}} is 0 for a spot fixation marker at \code{c(x,y)}, or 1 for a
#'         square centred on one of \code{(-3,0)}, \code{(0,0)}, \code{(+3,0)}.
#'     }
#'     \item{\code{tracking_on}} is either 0 (tracking off) or 1 (tracking on).
#'   }
#'   Note: tracking will be relative to the PRL established with the fixation
#'   marker used at setup (call to OPI-OPEN), so when tracking is on you should
#'   use the same fixation location as in the setup.
#' }
#' @return
#' \subsection{Compass}{ 
#'   A list contining \code{error} which is \code{NULL} for success, or some string description for fail.
#' }
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
#' @rdname opiClose
#' @return
#' \subsection{Compass}{
#'   Returns a list of \code{err}, which is an error code, and \code{fixations},
#'   which is a matrix with three columns: \code{time} (same as \code{time_hw}
#'   in \code{opiPresent}), \code{x} (degrees relative to the centre of the image
#'   returned by \code{opiInitialise} - not the PRL), \code{y} (as for x), and one row
#'   per fixation.
#' }
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
#' @rdname opiQueryDevice
#' @details
#' \subsection{Compass}{
#'   Return a list of all the constants used in the OPI Compass module.
#' }
#' @return
#' \subsection{Compass}{
#'   A list containing constants and their valuse used in the OPI Compass module.
#' }
compass.opiQueryDevice <- function() {
    return(lapply(ls(.OpiEnv$Compass), function(v) c(as.character(v), get(v, .OpiEnv$Compass))))
}

#
# OPI for Kowa AP 7000
# 
# Based on octopus900Client.r.
# 
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: December 2014
#
# Copyright [2015] [Andrew Turpin]
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

###################################################################
# .OpiEnv$KowaAP7000$socket is the connection to the AP 7000
# .OpiEnv$KowaAP7000$...    a variety of constants for colors, etc
###################################################################
if (exists(".OpiEnv") && !exists("KowaAP7000", where=.OpiEnv)) {
    assign("KowaAP7000", new.env(25), envir=.OpiEnv)

    .OpiEnv$KowaAP7000$BACKGROUND_WHITE  <- 0  # white, 10 cd/m^2
    .OpiEnv$KowaAP7000$BACKGROUND_YELLOW <- 1  # yellow, 100 cd/m^2

    .OpiEnv$KowaAP7000$FIX_CENTRE   <- 0   # fixation markers
    .OpiEnv$KowaAP7000$FIX_CENTER   <- 0   # usa spelling
    .OpiEnv$KowaAP7000$FIX_AUX      <- 1
    .OpiEnv$KowaAP7000$FIX_MACULA   <- 2
    .OpiEnv$KowaAP7000$FIX_AUX_LEFT <- 3

    .OpiEnv$KowaAP7000$SIZES_DEGREES <- c(6.5, 13, 26, 52, 104) / 60 # Goldmann target sizes in degrees

    .OpiEnv$KowaAP7000$COLOR_WHITE <- 0
    .OpiEnv$KowaAP7000$COLOR_GREEN <- 1
    .OpiEnv$KowaAP7000$COLOR_BLUE  <- 2
    .OpiEnv$KowaAP7000$COLOR_RED   <- 3

    # Utility functions for validating inputs
    .OpiEnv$KowaAP7000$minCheck <- function(x, limit, txt) {
        if (x < limit) {
	    opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
	}
    }
    .OpiEnv$KowaAP7000$maxCheck <- function(x, limit, txt) {
        if (x > limit) {
	    opiClose()
            stop(paste("opiPresent: ", txt, "is too big (maximum ", limit, ")"))
	}
    }
    .OpiEnv$KowaAP7000$checkOK <- function(txt) {
        res <- readLines(.OpiEnv$KowaAP7000$socket, n=1)
        #cat("ap7000 sends back>>>", res, "<<<\n")
        if (res != "OK")
            warning(paste(txt, "did not return OK from AP-7000"))
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
#' # KowaAP7000
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{KowaAP7000}, then you must specify
#'   the IP address and port of the AP-7000 server.
#'   
#'   * \code{ipAddress} is the IP address of the AP-7000 server as a string.
#'   * \code{port} is the TCP/IP port of the AP-7000 server as a number.
#' 
#' @return
#' ## Kowa AP-7000
#'   Always returns NULL.
#' 
#' @examples
#' \dontrun{
#'   # Set up the Kowa AP-7000
#'   chooseOpi("KowaAP7000")
#'   opiInitialize(ip="192.168.1.7", port=44965)
#' }
kowaAP7000.opiInitialize <- function(ip= "192.168.1.2", port=44965) {
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
        error=function(e) stop(paste("Cannot connect to AP 7000 at",ip,"on port", port))
    )

    assign("socket", socket, envir = .OpiEnv$KowaAP7000)
    msg <- paste0("OPI-SET-MODE\r")
    writeLines(msg, socket)
    res <- readLines(.OpiEnv$KowaAP7000$socket, n=1)

    if (res != "OK")
        stop(paste("Trouble initialising AP-7000. OPI-SET-MODE returns ",res))
    
    return(NULL)
}

#' @rdname opiPresent
#' @details
#' \subsection{KowaAP7000}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#' 
#'   If the chosen OPI implementation is \code{KowaAP7000}, then \code{nextStim}
#'   is ignored. 
#' }
kowaAP7000.opiPresent <- function(stim, nextStim=NULL) { UseMethod("kowaAP7000.opiPresent") }
setGeneric("kowaAP7000.opiPresent")

kowaAP7000.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, time=NA, pupilX=NA, pupilY=NA))
    }

    if(min(abs(.OpiEnv$KowaAP7000$SIZES_DEGREES - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    if (!is.element(stim$color, c(.OpiEnv$KowaAP7000$COLOR_WHITE,
                                  .OpiEnv$KowaAP7000$COLOR_GREEN,
                                  .OpiEnv$KowaAP7000$COLOR_BLUE ,
                                  .OpiEnv$KowaAP7000$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
    }

    .OpiEnv$KowaAP7000$minCheck(stim$x, -80, "Stimulus x")
    .OpiEnv$KowaAP7000$maxCheck(stim$x,  80, "Stimulus x")
    .OpiEnv$KowaAP7000$minCheck(stim$y, -70, "Stimulus y")
    .OpiEnv$KowaAP7000$maxCheck(stim$y,  65, "Stimulus y")
    .OpiEnv$KowaAP7000$minCheck(stim$duration,  100, "Stimulus duration")
    .OpiEnv$KowaAP7000$maxCheck(stim$duration, 1200, "Stimulus duration")
    .OpiEnv$KowaAP7000$minCheck(stim$responseWindow,  stim$duration, "Stimulus responseWindow")
    .OpiEnv$KowaAP7000$maxCheck(stim$responseWindow,           5000, "Stimulus responseWindow")
    .OpiEnv$KowaAP7000$minCheck(stim$level,  10000/pi/10^5, "Stimulus level")
    .OpiEnv$KowaAP7000$maxCheck(stim$level,  10000/pi     , "Stimulus level")

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, cdTodb(stim$level, 10000/pi))
    msg <- paste(msg, (which.min(abs(.OpiEnv$KowaAP7000$SIZES_DEGREES - stim$size))))
    msg <- paste(msg, stim$color)
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    msg <- paste(msg, "\r", sep="")

    writeLines(msg, .OpiEnv$KowaAP7000$socket)
    res <- readLines(.OpiEnv$KowaAP7000$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiPresent failed")

    return(list(
      err=NULL,
      seen=ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time=as.numeric(s[3]), 
      pupilX=as.numeric(s[4]),
      pupilY=as.numeric(s[5]),
      purkinjeX=as.numeric(s[6]),
      purkinjeY=as.numeric(s[7])
    ))
}

########################################## 
# Present kinetic stim, return values 
########################################## 
kowaAP7000.opiPresent.opiKineticStimulus <- function(stim, ...) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, x=NA, y=NA))
    }

    if (length(grDevices::xy.coords(stim$path)$x) > 2) 
        warning("opiPresent (kinetic): Kowa AP-7000 only supports paths of length 2 (start and end).  Ignoring all but the first two elements of stim$path etc")

        # convert sizes to .OpiEnv$KowaAP7000$SIZES_DEGREES
    stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.OpiEnv$KowaAP7000$SIZES_DEGREES - s))
         if(abs(.OpiEnv$KowaAP7000$SIZES_DEGREES[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         } 
         return(i)
    })

    if (!is.element(stim$colors[1], c(.OpiEnv$KowaAP7000$COLOR_WHITE,
                                  .OpiEnv$KowaAP7000$COLOR_GREEN,
                                  .OpiEnv$KowaAP7000$COLOR_BLUE ,
                                  .OpiEnv$KowaAP7000$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
     }

    .OpiEnv$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$x[1], -80, "Start x")
    .OpiEnv$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$x[1], 80, "Start x")
    .OpiEnv$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$x[2], -80, "End x")
    .OpiEnv$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$x[2], 80, "End x")
    .OpiEnv$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$y[1], -70, "Start y")
    .OpiEnv$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$y[1], 65, "Start y")
    .OpiEnv$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$y[2], -70, "End y")
    .OpiEnv$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$y[2], 65, "End y")
    .OpiEnv$KowaAP7000$minCheck(stim$levels[1],  10000/pi/10^5, "Stimulus level")
    .OpiEnv$KowaAP7000$maxCheck(stim$levels[1],  10000/pi     , "Stimulus level")
    .OpiEnv$KowaAP7000$minCheck(stim$speeds[1],  3, "Stimulus speed")
    .OpiEnv$KowaAP7000$maxCheck(stim$speeds[1],  5, "Stimulus speed")

    msg <- "OPI-PRESENT-KINETIC "
    xs <- grDevices::xy.coords(stim$path)$x[1]
    ys <- grDevices::xy.coords(stim$path)$y[1]
    msg <- paste(msg, grDevices::xy.coords(stim$path)$x[1])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$y[1])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$x[2])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$y[2])
    msg <- paste(msg, cdTodb(stim$levels[1], maxStim=10000/pi))
    msg <- paste(msg, stim$sizes[1])
    msg <- paste(msg, stim$colors[1])
    msg <- paste(msg, stim$speeds[1])
	msg <- paste(msg, "\r", sep="")
   
    writeLines(msg, .OpiEnv$KowaAP7000$socket)
    res <- readLines(.OpiEnv$KowaAP7000$socket, n=1)
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
kowaAP7000.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    opiClose()
    stop("opiPresent: Kowa AP 7000 does not support temporal stimuli")
}#opiPresent.opiTemporalStimulus()

###########################################################################
# set background color and/or fixation marker
# color is one of .OpiEnv$KowaAP7000$BACKGROUND_WHITE or 
#                 .OpiEnv$KowaAP7000$BACKGROUND_YELLOW
###########################################################################
#' @rdname opiSetBackground
#' @details
#' # KowaAP7000
#'   \code{opiSetBackground(lum, color, fixation)} 
#' 
#'   \code{lum} and \code{color} are dependant for the Kowa AP-7000. A white
#'   background must be 10 cd/\eqn{\mbox{m}^2}{m^2}, and a yellow background must
#'   be 100 cd/\eqn{\mbox{m}^2}{m^2}.
#' 
#'   If \code{lum} is 10 and \code{color} is not set, then
#'   \code{.OpiEnv$KowaAP7000$BACKGROUND_WHITE} is assumed.
#'   
#'   If \code{lum} is 100 and \code{color} is not set,
#'   then \code{.OpiEnv$KowaAP7000$BACKGROUND_YELLOW} is assumed.
#'   
#'   If both \code{lum} and \code{color} is set, then \code{lum} is ignored
#'   (a warning will be generated
#'   
#'   if \code{lum} is incompatible with \code{color}).
#'   
#'   \code{fixation} is one of
#'     * \code{.OpiEnv$KowaAP7000$FIX_CENTER}, fixation marker in the centre.
#'     * \code{.OpiEnv$KowaAP7000$FIX_CENTRE}, fixation marker in the centre.
#'     * \code{.OpiEnv$KowaAP7000$FIX_AUX},    fixation marker is ???.
#'     * \code{.OpiEnv$KowaAP7000$FIX_MACULA}, fixation marker is a circle(?).
#'     * \code{.OpiEnv$KowaAP7000$FIX_AUX_LEFT}, fixation marker is as for AUX but only lower left.
#' 
#' @return
#' ## KowaAP7000
#'   Always returns NULL
#' 
kowaAP7000.opiSetBackground <- function(lum=NA, color=NA, fixation=NA) {
    if (!is.na(fixation)) {
        .OpiEnv$KowaAP7000$minCheck(fixation, 0, "Fixation")
        .OpiEnv$KowaAP7000$maxCheck(fixation, 3, "Fixation")

        msg <- paste("OPI-SET-FIXATION ", fixation, "\r", sep="")
        writeLines(msg, .OpiEnv$KowaAP7000$socket)
        .OpiEnv$KowaAP7000$checkOK("opiSetBackground fixation")
    }

    if (!is.na(lum) && !is.na(color)) {
        if (lum == 10 && color != .OpiEnv$KowaAP7000$BACKGROUND_WHITE)
            warning("Can only have a 10 cd/m^2 background that is white")
        if (lum == 100 && color != .OpiEnv$KowaAP7000$BACKGROUND_YELLOW)
            warning("Can only have a 100 cd/m^2 background that is yellow")
    }

    if (!is.na(lum) && is.na(color)) {
        if (lum == 10) {
            color <- .OpiEnv$KowaAP7000$BACKGROUND_WHITE
            warning("Can only have a 10 cd/m^2 background that is white")
        } else if (lum == 100) {
            color <- .OpiEnv$KowaAP7000$BACKGROUND_YELLOW
            warning("Can only have a 100 cd/m^2 background that is yellow")
        } else {
            opiClose()
            stop("opiSetBackground: Can only have 10 cd/m^2 (white) or 100 cd/m^2 (yellow)")
        }
    }
    
    if (!is.na(color)) {
        .OpiEnv$KowaAP7000$minCheck(color, 0, "Background color")
        .OpiEnv$KowaAP7000$maxCheck(color, 1, "Background color")
        msg <- paste0("OPI-SET-BACKGROUND ", color,"\r")
        writeLines(msg, .OpiEnv$KowaAP7000$socket)
        .OpiEnv$KowaAP7000$checkOK("opiSetBackground color")
    }
        
    return(NULL)
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
#' @rdname opiClose
#' @return
#' \subsection{KowaAP7000}{
#'   DETAILS
#' }
kowaAP7000.opiClose <- function() {
    writeLines("OPI-CLOSE\r", .OpiEnv$KowaAP7000$socket)
    .OpiEnv$KowaAP7000$checkOK("opiClose")
    close(.OpiEnv$KowaAP7000$socket)
    return(NULL)
}

###########################################################################
# Lists defined constants
###########################################################################
#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @details
#' # KowaAP7000
#'   If the chosen OPI is \code{KowaAP7000}, then this function returns the current
#'   location of the pupil. See the Value section for details.
#'
#' # KowaAP7000
#'   Returns a list of 4 items:
#'   * \code{pupilX}, the x-coordinate of the pupil position in pixels.
#'   * \code{pupilY}, the y-coordinate of the pupil position in pixels.
#'   * \code{purkinjeX}, the x-coordinate of the purkinje position in pixels.
#'   * \code{purkinjeY}, the y-coordinate of the purkinje position in pixels.
#' 
#'   It also prints a list of constants that OPI knows about for the AP-7000.
#' 
kowaAP7000.opiQueryDevice <- function() {
    cat("Defined constants and functions\n")
    cat("-------------------------------\n")
    ls(envir=.OpiEnv$KowaAP7000)

    writeLines("OPI-GET-PUPILPOS\r", .OpiEnv$KowaAP7000$socket)
    res <- readLines(.OpiEnv$KowaAP7000$socket, n=1)
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

#
# OPI for Kowa AP 7000
# 
# Based on octopus900Client.r.
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: December 2014
#
# Copyright 2014 Andrew Turpin
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
# .KowaAP7000Env$socket is the connection to the AP 7000
# .KowaAP7000Env$...    a variety of constants for colors, etc
###################################################################
if (!exists(".KowaAP7000Env")) {
    .KowaAP7000Env <- new.env()

    .KowaAP7000Env$MODE_WoW <- 0  # white-on-white
    .KowaAP7000Env$MODE_BoY <- 1  # blue-on-yellow

    .KowaAP7000Env$FIX_CENTRE  <- 0   # fixation markers
    .KowaAP7000Env$FIX_CENTER  <- 0   # usa spelling
    .KowaAP7000Env$FIX_SQUARE  <- 1
    .KowaAP7000Env$FIX_DIAMOND <- 2

    .KowaAP7000Env$SIZES_DEGREES <- c(6.5, 13, 26, 52, 104) / 60 # Goldmann target sizes in degrees
}

#######################################################################
# INPUT: 
#   ip                       = ip address on which server is listening
#   port                     = port number on which server is listening
#   mode                     = .KowaAP7000Env$MODE_WoW or .KowaAP7000Env$MODE_BoY
#
# @return NULL if succeed
# @return 1    server not found/ready at the ip+port provided
#######################################################################
kowaAP7000.opiInitialize <- function(ip="192.168.1.7", port=50001, mode=NA) {
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

    if (is.na(mode) || (mode != .KowaAP7000Env$MODE_WoW && mode != .KowaAP7000Env$MODE_BoY))
        stop("You must specify which mode (.KowaAP7000Env$MODE_WoW or .KowaAP7000Env$MODE_BoY) in your call to opiInitialize")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to AP 7000 at",ip,"on port", port))
    )

    assign("socket", socket, envir = .KowaAP7000Env)
    msg <- paste0("OPI_SET_MODE ",mode)
    writeLines(msg, socket)
    res <- readLines(socket, n=1)
    
	if (res == "0")     
		return(NULL)
	else
		return(1)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#
# Return a list of 
#	err  = string message
#	seen = 1 if seen, 0 otherwise
#	time = reaction time
#   xs = list of x-coordinates of pupil position during presentation
#   ys = list of y-coordinates of pupil position during presentation
###########################################################################
kowaAP7000.opiPresent <- function(stim, nextStim=NULL) { UseMethod("kowaAP7000.opiPresent") }
setGeneric("kowaAP7000.opiPresent")

kowaAP7000.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if(min(abs(.KowaAP7000Env$SIZES_DEGREES - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    msg <- "OPI_PRESENT_STATIC "
    if (is.null(stim)) {
        msg <- paste(msg, "NULL")
    } else {
        msg <- paste(msg, stim$x, stim$y, cdTodb(stim$level, 10000/pi))
        msg <- paste(msg, (which.min(abs(.KowaAP7000Env$SIZES_DEGREES - stim$size))))
        msg <- paste(msg, stim$duration)
	    msg <- paste(msg, stim$responseWindow)
        if (!is.null(nextStim)) {
            msg <- paste(msg, nextStim$x, nextStim$y)
        }
    }

    writeLines(msg, .KowaAP7000Env$socket)
    res <- readLines(.KowaAP7000Env$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]
    if (s[1] == "null") {
      err  <- s[1]
      seen <- as.numeric(s[2])
      time <- as.numeric(s[3])
      coords <- as.numeric(strsplit(s[4], " ", fixed=T)[[1]])
      n <- coords[1]
      if (length(coords) != 1 + n + n)
        warning("AP 7000 server did not return a valid pupil position message")
      xs <- coords[2:(2+n-1)]
      ys <- coords[(2+n):(2+n+n-1)]
    } else {
      err <- s[1]
      seen <- time <- xs <- ys <- NA
    }

    return(list(
      err=err,
      seen=seen, 
      time=time, 
      xs=xs,
      ys=ys
    ))
}

########################################## 
# Present kinetic stim, return values 
########################################## 
kowaAP7000.opiPresent.opiKineticStimulus <- function(stim, ...) {
        # convert sizes to .KowaAP7000Env$SIZES_DEGREES
     stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.KowaAP7000Env$SIZES_DEGREES - s))
         if(abs(.KowaAP7000Env$SIZES_DEGREES[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         } 
         return(i)
     })

    msg <- "OPI_PRESENT_KINETIC "
    if (is.null(stim)) {
        msg <- paste(msg, "NULL")
    } else {
        xs <- xy.coords(stim$path)$x
        ys <- xy.coords(stim$path)$y
        msg <- paste(c(msg, length(xs), xs, ys), collapse=" ")
        msg <- paste(c(msg, sapply(stim$levels, cdTodb, maxStim=10000/pi)), collapse=" ")
        msg <- paste(c(msg, stim$sizes), collapse=" ")
        
          # convert seconds/degree into total time for path segment in seconds
        pathLengths <- NULL
        for(i in 2:length(xs)) {
          d <- sqrt((xs[i]-xs[i-1])^2 + (ys[i]-ys[i-1]^2))
          stim$speeds[i-1] <- d/stim$speeds[i-1]
        }
        msg <- paste(c(msg, stim$speeds), collapse=" ")  
    }
    
    writeLines(msg, .KowaAP7000Env$socket)
    res <- readLines(.KowaAP7000Env$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]

    if (s[1] == "null") {
      err <- NULL
    } else {
      err <- s[1]
    }

    return(list(
        err =err, 
        seen=strtoi(s[2]),
        time=strtoi(s[3]),
        x=strtoi(s[4]),     
        y=strtoi(s[5])
    ))
}

###########################################################################
# Not supported on AP 7000
###########################################################################
kowaAP7000.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("opiPresent: Kowa AP 7000 does not support temporal stimuli")
}#opiPresent.opiTemporalStimulus()

###########################################################################
# lum and color are ignored
# fiaxtion is one of
###########################################################################
kowaAP7000.opiSetBackground <- function(lum=NA, color=NA, fixation=NA) {
    if (!is.na(lum))
        warning("AP 7000 does not allow dynamic setting of background luminance. Use opiInitialise.")
    if (!is.na(color))
        warning("AP 7000 does not allow dynamic setting of background color. Use opiInitialise.")

    if (is.na(fixation) || ( 
       (fixation != .KowaAP7000Env$FIX_CENTRE) && 
       (fixation != .KowaAP7000Env$FIX_DIAMOND) && 
       (fixation != .KowaAP7000Env$FIX_SQUARE)))
        stop("opiSetBackground for the AP 7000 expects fixation to be one of 
.KowaAP7000Env$FIX_CENTRE, .KowaAP7000Env$FIX_DIAMOND, or .KowaAP7000Env$FIX_SQUARE")

    msg <- paste("OPI_SET_FIXATION", fixation)
    writeLines(msg, .KowaAP7000Env$socket)
    ret <- strtoi(readLines(.KowaAP7000Env$socket, n=1))

    if (ret == 0) {
        return(NULL)
    } else {
        return(ret)
    }
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
kowaAP7000.opiClose <- function() {
    writeLines("OPI_CLOSE", .KowaAP7000Env$socket)
    close(.KowaAP7000Env$socket)
    return(NULL)
}

###########################################################################
# Lists defined constants
###########################################################################
kowaAP7000.opiQueryDevice <- function() {
    cat("Defined constants and functions\n")
    cat("-------------------------------\n")
    ls(envir=.KowaAP7000Env)

    return(NULL)
}

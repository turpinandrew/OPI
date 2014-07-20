#
# OPI for Octopus 900 
# 
# This would all have been nicer in an OO style, with each implementation
# being a subclass of an opi class, but I don't think it can be in R.
# The OPI standard doesn't want users employing exactly the same function 
# no matter what the underlying implementation, and so there cannot be 
# extra parameters to create method signatures for different classes.
# Similarly, some implementations use exactly the same method signatures,
# again which will confuse R, I think. Anyway, if I am wrong, sorry about that.
# What I've done (use a list of implementations and then use a global
# integer to index them) works and makes sense to the non-OO person.
#
# This version creates a socket to O900Server.java and sends/receives 
# commands. It requires the server to be running, and the server 
# to know where the H-S jar files are (ie in the CLASSPATH). 
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Author: David Lawson    (XXX)
# Date: July 2014
#
# Copyright 2012 Andrew Turpin and David Lawson
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
# .Octopus900Env$octopusObject is the java Opi object set in opiInitialize
# .Octopus900Env$... are lots of colors and Fixation constants set in setupBackgroundConstants()
###################################################################
if (!exists(".Octopus900Env"))
    .Octopus900Env <- new.env()

###########################################################################
# Get values for fixation, color and bg intensity constants
# from EyeSuite classes, and set globals
#       .Octopus900Env$* 
# to the values of those constants.
# INPUT: None.
# OUTPUT: None.
# SIDE EFFECTS: sets .Octopus900Env$* if possible.
###########################################################################
setupBackgroundConstants <- function() {
    # TODO add a server command to return these
    warning("setupBackgroundConstants not yet written in octopus900Client.r")
}


###################################################################
# Goldmann target sizes in degrees
###################################################################
GOLDMANN <- c(6.5, 13, 26, 52, 104) / 60


    # uncomment for Tony's big wheel
#mm <- c(0.125,0.25,0.5,1,1.41,2,2.83,4,5.66,8,11.3,16,22.6,32,64,128,256)
#ind <- c(32,28,31,26,30,29,27,24,25,23,21,22,39,38,20,37,36)
#GOLDMANN <- rep(NA,39)
#GOLDMANN[ind] <- (sqrt(mm/pi)*180/pi/149.1954)

#######################################################################
# INPUT: 
#   serverPort               = port number on which server is listening
#   eyeSuiteSettingsLocation = dir name containing EyeSuite settings
#   eye                      = "right" or "left"
#   gazeFeed                 = 0 (none), 1 (single frame), 2 (all frames with *)
#
#   Both input dirs should INCLUDE THE TRAILING SLASH.
#
# @return NULL if succeed
# @return 1 if already initialised
# @return 2 if failed to make ready
#
#######################################################################
octo900.opiInitialize <- function(serverPort=50001,eyeSuiteSettingsLocation=NA, eye=NA, gazeFeed=0) {
    if (is.na(eyeSuiteJarLocation))
        stop("You must specify the EyeSuite jar file folder in your call to opiInitialize")
    if (is.na(eyeSuiteSettingsLocation))
        stop("You must specify the EyeSuite settings folder in your call to opiInitialize")
    if (is.na(eye))
        stop("You must specify which eye ('left' or 'right') in your call to opiInitialize")
    if (eye != "left" && eye != "right")
        stop("The eye argument of opiInitialize must be 'left' or 'right'")
    if (gazeFeed != 0)
        stop("Gaze tracking not yet supported with client/server model")


    setupBackgroundConstants()

    socket <- tryCatch(
        socketConnection(host = "127.0.0.1", serverPort, open = "w+b", blocking = TRUE, timeout = 120), 
        error <- function(e) stop(paste("Cannot connect to Octopus 900 on port", serverPort))
    )

    assign("socket", socket, envir = .Octopus900Env)
    msg <- paste0("OPI_INITIALIZE \"",eyeSuiteSettingsLocation,"\ ",eye, " ", gazeFeed)
    writeLines(msg, socket)
    res <- readLines(socket, n=1)

	if (res == "0")
		return(NULL)
	else
		return(res)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#
# Return a list of 
#	err  = string message
#	seen = 1 if seen, 0 otherwise
#	time = reaction time
###########################################################################
octo900.opiPresent <- function(stim, nextStim=NULL) { UseMethod("octo900.opiPresent") }
setGeneric("octo900.opiPresent")

octo900.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if(min(abs(GOLDMANN - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    msg <- "OPI_PRESENT_STATIC "
    if (is.null(stim)) {
        msg <- paste(msg, "NULL")
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0, cdTodb(stim$level, 4000/pi) * 10.0)
        msg <- paste(msg, (which.min(abs(GOLDMANN - stim$size))))
        msg <- paste(msg, stim$duration)
	    msg <- paste(msg, stim$responseWindow)
        if (!is.null(nextStim)) {
            msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
        }
    }

    writeLines(msg, .Octopus900Env$socket)
    res <- readLines(.Octopus900Env$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]
    return(list(
	    err =s[1], 
	    seen=s[2],
	    time=s[3],
	    frames=NA,
        numFrames=NA,
        width=NA,
        height=NA
	))
}


###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#
# Return a list of 
#	err  = string message
#	seen = 1 if seen, 0 otherwise
#	time = reaction time
###########################################################################
octo900.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {


    if(min(abs(GOLDMANN - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    msg <- "OPI_PRESENT_TEMPORAL "
    if (is.null(stim)) {
        msg <- paste(msg, "NULL")
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0, stim$rate)
        msg <- paste(msg, (which.min(abs(GOLDMANN - stim$size))))
        msg <- paste(msg, stim$duration)
        msg <- paste(msg, stim$responseWindow)
        if (!is.null(nextStim)) {
            msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
        }
    }

    writeLines(msg, .Octopus900Env$socket)
    res <- readLines(.Octopus900Env$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]
    return(list(
        err =s[1], 
        seen=s[2],
        time=s[3],
        frames=NA,
        numFrames=NA,
        width=NA,
        height=NA
    ))

}#opiPresent.opiTemporalStimulus()

########################################## TO DO
octo900.opiPresent.opiKineticStimulus <- function(stim, ...) {
 #    if (is.null(stim)) {
 #        stimObj <- .jnull("opi/OpiTemporalStimulus")
 #    } else { 
 #            # convert sizes to GOLDMANN
 #        stim$sizes <- sapply(stim$sizes, function(s) {
 #            i <- which.min(abs(GOLDMANN - s))
 #            if(abs(GOLDMANN[i] - s) > 0.000001) {
 #                warning("opiPresent: Rounding stimulus size to nearest Goldmann size")
 #            } 
 #            return(i)
 #        })

 #            # bit of a kludge as passing vector of one double seemed to barf
 #        if (length(stim$path$x) == 2)
 #            stimObj <- .jnew("opi/OpiKineticStimulus", 
 #                sapply(stim$path$x, as.double), 
 #                sapply(stim$path$y, as.double), 
 #                as.double(cdTodb(stim$levels[1])),
 #                as.double(stim$sizes[1]),
 #                as.double(stim$speeds[1]))
 #        else
 #            stimObj <- .jnew("opi/OpiKineticStimulus", 
 #                sapply(stim$path$x, as.double), 
 #                sapply(stim$path$y, as.double), 
 #                as.vector(sapply(sapply(stim$levels, cdTodb), as.double)),
 #                as.vector(sapply(stim$sizes, as.double)), 
 #                as.vector(sapply(stim$speeds, as.double)))
 #    }

 #    done <- FALSE
 #    while (!done) {
	# done <- TRUE
 #    	tryCatch(ret <- .jcall(.Octopus900Env$octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj), 
	#              java.util.ConcurrentModificationException = function(e) { done = FALSE })
 #    }

 #    return(list(
	#     err =.jcall(ret, "S", "getErr"), 
	#     seen=ifelse(.jcall(ret, "I", "getSeen") == 0, 0, 1),
	#     time=.jcall(ret, "I", "getTime")
	# ))
    return(NULL)
}

###########################################################################
#
# Input paras are the Octopus900Env$* constants
# lum is in cd/m^2 (as per OPI spec) * 100 == .Octopus900Env$BG_{OFF | 1 | 10 | 100 }
# color is .Octopus900Env$MET_COL_{WW | BY | RW | BLUE_WHITE | RED_YELLOW | WHITE_YELLOW }
# fixation is .Octopus900Env$FIX_{RING | CROSS | CENTRE}
# fixIntensity is 0..100 %
#
# @return NULL is succeed.
# @return -1 if opiInitialize has not been successfully called
# @return -2 trouble setting backgound color
# @return -3 trouble setting fixation
###########################################################################
octo900.opiSetBackground <- function(lum="NA", color="NA", fixation="NA", fixIntensity=50) {

    msg <- paste("OPI_SET_BACKGROUND", lum, color, fixation, fixIntensity)
    writeLines(msg, .Octopus900Env$socket)
    ret <- strtoi(readLines(.Octopus900Env$socket, n=1))

    if (ret == 0) {
        return(NULL)
    } else {
        return(ret)
    }
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
octo900.opiClose <- function() {
    writeLines("OPI_CLOSE", .Octopus900Env$socket)
    close(.Octopus900Env$socket)
    return(NULL)
}

###########################################################################
# Call opiPresent with a NULL stimulus
###########################################################################
octo900.opiQueryDevice <- function() {
    ret <- octo900.opiPresent.opiStaticStimulus(NULL, NULL)
    return(ret$err)
}

#
# OPI for Google Daydream
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: Mar 2019 
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
# .DayDreamEnv$degrees_to_pixels() function(x,y) in degrees to (x,y) 
# .DayDreamEnv$...    a variety of constants, etc
###################################################################
if (!exists(".DayDreamEnv")) {
    .DayDreamEnv <- new.env()

    .DayDreamEnv$endian <- "little"

    .DayDreamEnv$LUT <- NULL
    .DayDreamEnv$degrees_to_pixels <- NULL

    .DayDreamEnv$background_left <- NA    # NA or an array[1..3,1..w,1..h] if set
    .DayDreamEnv$background_right <- NA

    .DayDreamEnv$width <- NA        # of whole phone screen
    .DayDreamEnv$height <- NA
    .DayDreamEnv$single_width <- NA    # just one eye image
    .DayDreamEnv$single_height <- NA

    .DayDreamEnv$SEEN     <- 1  
    .DayDreamEnv$NOT_SEEN <- 0  
}

#######################################################################
#' Prepare to send commands to Daydream.
#'
#' As per the OPI standard, `opiInitialise` sets up the environment 
#' and opens a socket to Daydream.
#'
#' For this function to work correctly, parameters must 
#' be named (as in all OPI functions).
#'
#' @param ip   IP address on which server is listening.
#' @param port Port number on which server is listening.
#' @param lut  \code{lut[i]} is cd/m^2 for grey level i. \code{assert(length(lut) == 256)}
#' @param degrees_to_pixels  \code{function(x,y)} where x and y are in degrees, 
#'                           returns c(x,y) in pixels for one eye.
#'
#' @return \code{list(err = NULL)} if succeed, will stop otherwise.
#'
#' @examples
#' \dontrun{opiInitialise(ip="10.0.1.1", port=8912)}
#'
#' @rdname opiInitialise
#######################################################################
daydream.opiInitialize <- function(
        ip="127.0.0.1",
        port=50008, 
        lut = rep(1000, 256),
        degrees_to_pixels = function(x,y) return(20*c(x,y))
    ) {
    cat("Looking for phone at ", ip, "\n")
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
    assign("degrees_to_pixels", degrees_to_pixels, envir = .DayDreamEnv)

    writeLines("OPI_GET_RES", .DayDreamEnv$socket)
    assign("width",        readBin(.DayDreamEnv$socket, "integer", size=4, endian=.DayDreamEnv$endian), envir=.DayDreamEnv)
    assign("height",       readBin(.DayDreamEnv$socket, "integer", size=4, endian=.DayDreamEnv$endian), envir=.DayDreamEnv)
    assign("single_width", readBin(.DayDreamEnv$socket, "integer", size=4, endian=.DayDreamEnv$endian), envir=.DayDreamEnv)
    assign("single_height",readBin(.DayDreamEnv$socket, "integer", size=4, endian=.DayDreamEnv$endian), envir=.DayDreamEnv)

    return(list(err=NULL))
}

###########################################################################
# Find the closest pixel value (index into .DayDreamEnv$LUT less 1)
# for cd/m^2 param cdm2
###########################################################################
find_pixel_value <- function(cdm2) {
    return (which.min(abs(.DayDreamEnv$LUT - cdm2) - 1))
}

###########################################################################
# Load an image of a stimuli with background given by 
# .DayDreamEnv$background_{eye} and forgraound given by pv.
# The stim is a circle at (x,y) which are in degrees.
#
# Assumes .DayDreamEnv$background_{eye} is set.
#
# @param fg integer in range [0,255]
# @param eye is 'L' or 'R'
#
# @return TRUE if succeeds, FALSE otherwise
###########################################################################
load_image <- function(size, x, y, fg, eye) {
    if (eye == 'L') {
        im <- .DayDreamEnv$background_left
    } else {
        im <- .DayDreamEnv$background_right
    }

    h <- dim(im)[1]
    w <- dim(im)[2]     # assert(dim(im)[3] == 3)

    radius <- round(mean(.DayDreamEnv$degrees_to_pixels(size/2, size/2)))
    xy <- .DayDreamEnv$degrees_to_pixels(x, y)

    for (ix in -radius:radius)
        for (iy in -radius:radius)
            if (ix^2 + iy^2 <= radius)
                im[xy[2] + iy, xy[1] + ix, ] <- pv      # BUG! prob wrong indexes 

    writeLines(paste("OPI_IMAGE", w, h), .DayDreamEnv$socket)
    res <- readLines(.DayDreamEnv$socket, n=1)
    if (res == "READY") {
        for (iy in 1:h)
            for (ix in 1:w)
                writeBin(as.integer(rep(im[iy,ix,])), .DayDreamEnv$socket, size=3*4, endian=.DayDreamEnv$endian)
    } else {
        return(FALSE)
    }

    res <- readLines(.DayDreamEnv$socket, n=1)

    return (res == "OK")
}

###########################################################################
# INPUT: 
#   As per OPI spec. Note eye is part of stim object
#
# Return a list of 
#    err             : (integer) 0 all clear, >= 1 some error codes (eg cannot track, etc)
#    seen            : 0 for not seen, 1 for seen (button pressed in response window)
#    time            : in ms (integer) (does this include/exclude the 200ms presentation time?) -1 for not seen.
###########################################################################
daydream.opiPresent <- function(stim, nextStim=NULL) { UseMethod("daydream.opiPresent") }
setGeneric("daydream.opiPresent")

daydream.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) return(list(err="The NULL stimulus not supported", seen=NA, time=NA))

    if (is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$size)) return(list(err="No size in stimulus", seen=NA, time=NA))
    if (is.null(stim$level)) return(list(err="No level in stimulus", seen=NA, time=NA))
    if (is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
    if (is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))
    if (is.null(stim$eye)) return(list(err="No eye in stimulus", seen=NA, time=NA))

    if (stim$eye == "R" && !.DayDreamEnv$background_right)
        return(list(err="Background image for right eye not set.", seen=NA, time=NA))

    if (stim$eye == "L" && !.DayDreamEnv$background_left)
        return(list(err="Background image for left eye not set.", seen=NA, time=NA))

    if (!is.null(nextStim)) 
        warning("opiPresent: nextStim ignored")

    pv <- find_pixel_value(stim$level)
    if (load_image(stim$size, stim$x, stim$y, pv, stim$eye)) {
        msg <- paste("OPI_PRESENT", stim$eye, stim$duration, stim$responseWindow, sep=" ")
        writeLines(msg, .DayDreamEnv$socket)

        seen <- readBin(.DayDreamEnv$socket, "character", size=1) == '1'
        time <- readBin(.DayDreamEnv$socket, "double", size=4, endian=.DayDreamEnv$endian)

        if (!seen && time == 0)
            return(list(err="Background image not set", seen=NA, time=NA))
        if (!seen && time == 1)
            return(list(err="OPI present error back from daydream", seen=NA, time=NA))

        return(list(
            err  =NULL,
            seen =seen,    # assumes 1 or 0, not "true" or "false"
            time =time
        ))
    } else {
        return(list(err="OPI present could not load stimulus image", seen=NA, time=NA))
    }
}

########################################## 
# Present kinetic stim, return values 
########################################## 
daydream.opiPresent.opiKineticStimulus <- function(stim, ...) {
    warning("DayDream does not support kinetic stimuli (yet)")
    return(list(err="DayDream does not support kinetic stimuli (yet)", seen=FALSE, time=0))
}

###########################################################################
# Not supported on AP 7000
###########################################################################
daydream.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("DayDream does not support temporal stimuli (yet)")
    return(list(err="DayDream does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

###########################################################################
# Used to turn tracking on or off or alter fixation.
###########################################################################
daydream.opiSetBackground <- function(lum=NA, color=c(128,128,128), eye="L") {
    w <- .DayDreamEnv$single_width
    h <- .DayDreamEnv$single_height

    print(paste("setbg",w,h))

    if (!is.na(lum)) {
        if (!is.na(color))
            warning("opiSetBackground: Ignoring color and using lum")
        a <- array(find_pixel_value(lum), dim=c(3, w, h))
    } else {
        a <- array(color, dim=c(3, w, h))
    }

    writeLines(paste("OPI_IMAGE", w, h), .DayDreamEnv$socket)
    res <- readLines(.DayDreamEnv$socket, n=1)
    print(res)
    if (res == "READY") {
        writeBin(as.integer(a[,,]), .DayDreamEnv$socket, size=1, endian=.DayDreamEnv$endian)
    } else {
        return(-1)
    }

    res <- readLines(.DayDreamEnv$socket, n=1)

    if (res != "OK")
        return(-2)

    writeLines(paste("OPI_SET_BACKGROUND", eye), .DayDreamEnv$socket)
    res <- readLines(.DayDreamEnv$socket, n=1)
    if (res == "ERR")
        return(-3)
    
    if (eye == 'L')
        assign("background_left", a, envir=.DayDreamEnv)
    else
        assign("background_right", a, envir=.DayDreamEnv)

    return(NULL)
}

##############################################################################
#### return list(err=NULL, fixations=matrix of fixations)
####       matrix has one row per fixation
####       col-1 timestamp (ms since epoch) 
####       col-2 x in degrees 
####       col-3 y in degrees 
##############################################################################
###compass.opiClose <- function() {
###    writeLines("OPI-CLOSE", .CompassEnv$socket)
###
###    num_bytes <- readBin(.CompassEnv$socket, "integer", size=4, endian=.CompassEnv$endian)
###    print(paste("Num bytes", num_bytes))
###
###    if (num_bytes == 0) {
###        warning("opiClose() returned no bytes - perhaps you forgot opiInitialise")
###        return(list(err="No Bytes"))
###    }
###
###    num_triples <- num_bytes/12
###    fixations <- matrix(NA, ncol=3, nrow=num_triples)
###    for(i in 1:num_triples) {
###        fixations[i,1] <- readBin(.CompassEnv$socket, "integer", n=1, size=4, endian=.CompassEnv$endian)
###        fixations[i,2:3] <- readBin(.CompassEnv$socket, "double", n=2, size=4,  endian=.CompassEnv$endian)
###    }
###
###    close(.CompassEnv$socket)
###
###    return(list(err=NULL, fixations=fixations))
###}
###
##############################################################################
#### Lists defined constants
##############################################################################
###compass.opiQueryDevice <- function() {
###    return(list(default="Nothing to report", isSim=FALSE))
###}
###

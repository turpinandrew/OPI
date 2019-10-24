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
# .OpiEnv$DayDream$socket is the connection to the daydream
# .OpiEnv$DayDream$LUT has 256 entries. LUT[x] is cd/m^2 value for grey level x
# .OpiEnv$DayDream$ppd default is 50.
# .OpiEnv$DayDream$...    a variety of constants, etc
###################################################################
if (exists(".OpiEnv") && !exists("DayDream", where=.OpiEnv)) {
    assign("DayDream", new.env(25), envir=.OpiEnv)

    .OpiEnv$DayDream$endian <- "little"

    .OpiEnv$DayDream$LUT <- NULL
    .OpiEnv$DayDream$ppd <- NULL

    .OpiEnv$DayDream$width <- NA        # of whole phone screen
    .OpiEnv$DayDream$height <- NA
    .OpiEnv$DayDream$single_width <- NA
    .OpiEnv$DayDream$single_height <- NA

    .OpiEnv$DayDream$background_left <- NA    # stored MONO background
    .OpiEnv$DayDream$background_right <- NA

    .OpiEnv$DayDream$SEEN     <- 1  
    .OpiEnv$DayDream$NOT_SEEN <- 0  
}

#' @rdname opiInitialize
#' @param lut Look up table mapping pixel values to cd/m2 for "Daydream"
#' @param ppd pixels per degree
#' @details
#' \subsection{Daydream}{
#'   \code{opiInitialize(ip="127.0.0.1", port=50008, lut= seq(0, 400, length.out = 255), ppd = 11)}
#'   
#'   If the chosen OPI implementation is \code{Daydream}, then you must specify
#'   the IP address of the Android phone that is in the Daydream, and the port on
#'   which the server running on the phone is listening.
#'   
#'   \itemize{
#'     \item\code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is \code{seq(0, 4000, length.out = 255)}
#'     \item\code{ppd} points per degree. It is a scalar to multiply to x and y coordinates to convert from degrees to pixels. This assumes the viewing distance (z-coordinate) is 30cm.
#'   }
#' }
#' @return
#' \subsection{Daydream}{
#'   DETAILS
#' }
daydream.opiInitialize <- function(
        ip="127.0.0.1",
        port=50008, 
        lut = seq(0, 400, length.out = 255), # for pixel 1 max brightness is 402
        ppd = 11                             # for pixel 1 and daydream view: daydream view has 960 horizontal pixels per eye divided by a FoV of 90 degrees
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

    assign("socket", socket, envir = .OpiEnv$DayDream)
    assign("LUT", lut, envir = .OpiEnv$DayDream)
    assign("ppd", ppd, envir = .OpiEnv$DayDream)

    writeLines("OPI_GET_RES", .OpiEnv$DayDream$socket)
    assign("width",        readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("height",       readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_width", readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_height",readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n

    print(paste("Phone res", .OpiEnv$DayDream$width, .OpiEnv$DayDream$height, .OpiEnv$DayDream$single_width, .OpiEnv$DayDream$single_height))
    return(list(err=NULL))
}

###########################################################################
# Find the closest pixel value (index into .OpiEnv$DayDream$LUT less 1)
# for cd/m^2 param cdm2
###########################################################################
find_pixel_value <- function(cdm2) {
    return (which.min(abs(.OpiEnv$DayDream$LUT - cdm2) - 1))
}

###########################################################################
# Load an image to server
#
# @param im is an array of RGB values dim=c(h,w,3)
#
# @return TRUE if succeeds, FALSE otherwise
###########################################################################
load_image <- function(im, w, h) {

    writeLines(paste("OPI_IMAGE", w, h), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res == "READY") {
        writeBin(as.raw(c(im)), .OpiEnv$DayDream$socket, size=1, endian=.OpiEnv$DayDream$endian)
    } else {
        return(FALSE)
    }
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    print(paste('Load image',res))
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
#' @rdname opiPresent
#' @details
#' \subsection{Daydream}{
#'   If the chosen OPI implementation is \code{Daydream}, then \code{nextStim}
#'   is ignored.
#'   
#'   Note that the dB level is rounded to the nearest cd/\eqn{\mbox{m}^2}{m^2}
#'   that is in the \code{lut} specified in \code{opiInitialise}.
#'   
#'   Currently uses the most simple algorithm for drawing a 'circle'
#'   (ie not Bresenham's).
#'   
#'   Currently only implemented for \code{opiStaticStimulus}.
#' }
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

    # make the stimulus (IMF by now it is just monochromatic white)
    cx <- round(.OpiEnv$DayDream$single_width / 2)
    cy <- round(.OpiEnv$DayDream$single_height / 2)
    xy <- c(cx,cy) + .OpiEnv$DayDream$ppd * c(stim$x, -stim$y) 

    bg <- ifelse(stim$eye == "L", .OpiEnv$DayDream$background_left, .OpiEnv$DayDream$background_right)
    radius <- round(mean(.OpiEnv$DayDream$ppd * stim$size / 2 * c(1, 1)))
    len <- 2 * radius + 1 # get length of the image
    npix <- len^2         # get number of pixels
    im <- matrix(bg, 3, npix)

    x <- 0:(npix - 1) %% len
    y <- 0:(npix - 1) %/% len

    im[,(x - radius)^2 + (y - radius)^2 <= radius^2] <- find_pixel_value(stim$level)
    if (load_image(im, len, len)) {
        msg <- paste("OPI_MONO_PRESENT", stim$eye, xy[1], xy[2], stim$duration, stim$responseWindow, sep=" ")
        writeLines(msg, .OpiEnv$DayDream$socket)

        seen <- readBin(.OpiEnv$DayDream$socket, "integer", size=1)
        time <- readBin(.OpiEnv$DayDream$socket, "double", size=4, endian=.OpiEnv$DayDream$endian)
        readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n

        cat("seen: "); print(seen)
        cat("time: "); print(time)
        seen <- seen == '1'

        if (!seen && time == 0)
            return(list(err="Background image not set", seen=NA, time=NA))
        if (!seen && time == 1)
            return(list(err="Trouble with stim image", seen=NA, time=NA))
        if (!seen && time == 2)
            return(list(err="Location out of range for daydream", seen=NA, time=NA))
        if (!seen && time == 3)
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
# Not supported on Daydream
###########################################################################
daydream.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("DayDream does not support temporal stimuli (yet)")
    return(list(err="DayDream does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

###########################################################################
# Used to set background for one eye.
# @param lum            cd/m^2
# @param color          Ignored for now
# @param fixation       Just 'Cross' for now
# @param fixation_color RGB triple with values in range [0,255]
# @param fixation_size  Length of cross hairs In pixels
# @param eye            "L" or "R"
#
# @return string on error
# @return NULL if everything works
###########################################################################
#' @rdname opiSetBackground
#' @param fixation_size fixation size for "Daydream"
#' @param fixation_color fixation color for "Daydream"
#' @param eye eye for "Daydream"
#' @details
#' \subsection{Daydream}{
#'   \code{opiSetBackground(lum=NA, color=NA, fixation="Cross", fixation_size=11, fixation_color=c(0,128,0), eye="L")}
#'   \itemize{
#'     \item{\code{lum}} in cd/\eqn{\mbox{m}^2}{m^2} is set to nearest grey value in \code{lut} from \code{opiInitialize}.
#'     \item{\code{color}} is ignored.
#'     \item{\code{fixation}} can only be \code{'Cross'} at the moment.
#'     \item{\code{fixation_size}} is the number of pixels one cross-hair is in
#'       length.
#'     \item{\code{fixation_color}} RGB value of coor of fixation cross. Values
#'       in range [0,255].
#'   }
#' }
#' @return
#' \subsection{Daydream}{ 
#'   DETAILS
#' }
daydream.opiSetBackground <- function(lum=NA, color=NA, fixation="Cross", 
        fixation_size=11,    # probably should be odd # this is 1 degree for the dyadream view + pixel 1
        fixation_color=c(0,255,0), 
        eye="L") {
    if (is.na(lum)) {
        warning('Cannot set background to NA in opiSetBackground')
        return('Cannot set background to NA in opiSetBackground')
    }
    if (!is.na(color)) { warning('Color ignored in opiSetBackground.') }

    bg <- find_pixel_value(lum)
    writeLines(paste("OPI_MONO_SET_BG", eye, bg), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res != "OK")
        return(paste0("Cannot set background to ",bg," in opiSetBackground"))

    if (eye == "L")
        .OpiEnv$DayDream$background_left <- bg
    else
        .OpiEnv$DayDream$background_right <- bg

    # if even, add 1 to fixation size
    if(fixation_size %% 2 == 0) fixation_size <- fixation_size + 1
    if (fixation == 'Cross') {
        # set fixation to a cross
        m <- (fixation_size + 1) / 2
        npix <- fixation_size^2
        im <- matrix(bg, 3, npix)
        x <- 0:npix %% fixation_size + 1
        y <- 0:npix %/% fixation_size + 1
        im[,which(x == m | y == m)] <- fixation_color
        if (!load_image(im, fixation_size, fixation_size))
            return("Trouble loading fixation image in opiSetBackground.")
        cx <- round(.OpiEnv$DayDream$single_width / 2)
        cy <- round(.OpiEnv$DayDream$single_height / 2)
        cat("Fixation at: ", eye, " ", cx, " ", cy, "\n")

        writeLines(paste("OPI_MONO_BG_ADD", eye, cx, cy), .OpiEnv$DayDream$socket)
        res <- readLines(.OpiEnv$DayDream$socket, n=1)
        if (res != "OK")
            return("Trouble adding fixation to background in opiSetBackground")
    }
    
    return(NULL)
}

##############################################################################
#### return list(err=NULL, fixations=matrix of fixations)
####       matrix has one row per fixation
####       col-1 timestamp (ms since epoch) 
####       col-2 x in degrees 
####       col-3 y in degrees 
##############################################################################
#' @rdname opiClose
#' @return
#' \subsection{Daydream}{
#'   DETAILS
#' }
daydream.opiClose <- function() {
    writeLines("OPI_CLOSE", .OpiEnv$DayDream$socket)

    res <- readLines(.OpiEnv$DayDream$socket, n=1)

    close(.OpiEnv$DayDream$socket)

    if (res != "OK")
        return(list(err="Trouble closing daydream connection."))
    else
        return(list(err=NULL))
}

##############################################################################
#### Lists defined constants
##############################################################################
#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @details
#' \subsection{Daydream}{
#'   Returns all constants in \code{.OpiEnv$DayDream} as a list.
#' }
#' \subsection{Daydream}{
#'   DETAILS
#' }
daydream.opiQueryDevice <- function() {
    vars <- ls(.OpiEnv$DayDream)
    lst <- lapply(vars, function(i) .OpiEnv$DayDream[[i]])
    names(lst) <- vars
    return(lst)
}
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
# .OpiEnv$DayDream$fovy default is 90
# .OpiEnv$DayDream$...    constants and setting variables
###################################################################
if (exists(".OpiEnv") && !exists("DayDream", where=.OpiEnv)) {
  assign("DayDream", new.env(25), envir=.OpiEnv)
  
  .OpiEnv$DayDream$endian <- "little"
  .OpiEnv$DayDream$socket <- NA

  .OpiEnv$DayDream$LUT <- NA # look up table
  .OpiEnv$DayDream$fovy <- NA # Field of view for y-axis

  .OpiEnv$DayDream$width <- NA        # of whole phone screen
  .OpiEnv$DayDream$height <- NA
  .OpiEnv$DayDream$single_width <- NA
  .OpiEnv$DayDream$single_height <- NA

  .OpiEnv$DayDream$background_left  <- NA    # stored MONO background information
  .OpiEnv$DayDream$background_right <- NA
  .OpiEnv$DayDream$background_color <- NA
  .OpiEnv$DayDream$fix_color        <- NA
  
  .OpiEnv$DayDream$SEEN     <- 1  
  .OpiEnv$DayDream$NOT_SEEN <- 0  
}

###########################################################################
# Find the closest pixel value (index into .OpiEnv$DayDream$LUT less 1)
# for cd/m^2 param cdm2
###########################################################################
find_pixel_value <- function(cdm2) {
  # It is 8 bit depth, so it must return a value from 0 to 255
  return(which.min(abs(.OpiEnv$DayDream$LUT - cdm2)) - 1)
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
  return (res == "OK")
}

#' @rdname opiInitialize
#' @param lut Look up table mapping pixel values to cd/m2
#' @param fovy Field of view in degrees in the y-axis. It is different depending on the device.
#' For Daydream view, it is 90 degrees, for, Daydream view 2 is 100 degrees. Default is 90.
#' @details
#' \subsection{Daydream}{
#'   \code{opiInitialize(ip="127.0.0.1", port=50008, lut= seq(0, 400, length.out = 256), fovy = 90)}
#'   
#'   If the chosen OPI implementation is \code{Daydream}, then you must specify
#'   the IP address of the Android phone that is in the Daydream, and the port on
#'   which the server running on the phone is listening.
#'   
#'   \itemize{
#'     \item\code{ip} is the IP address of the Daydream server as a string
#'     \item\code{port} is the TCP/IP port of the Daydream server as a number
#'     \item\code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the
#'       cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is
#'       \code{seq(0, 4000, length.out = 256)}
#'     \item\code{fovy} Field of view in degrees in the y-axis. It is different depending on the device.
#'       For Daydream view, it is 90 degrees, for, Daydream view 2 is 100 degrees. Default is 90.
#'   }
#' }
#' @return
#' \subsection{Daydream}{
#'   Always returns NULL.
#' }
daydream.opiInitialize <- function(
    ip="127.0.0.1",
    port=50008, 
    lut = seq(0, 400, length.out = 256), # for pixel 1 max brightness is 400
    fovy = 90
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
    assign("LUT",    lut,    envir = .OpiEnv$DayDream)
    assign("fovy",   fovy,   envir = .OpiEnv$DayDream)
    # set field of view in the y-axis in degrees of visual angle
    msg <- paste("OPI_SET_FOVY", fovy, sep=" ")
    writeLines(msg, .OpiEnv$DayDream$socket)
    writeLines("OPI_GET_RES", .OpiEnv$DayDream$socket)
    assign("width",        readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("height",       readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_width", readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_height",readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n
    print(paste("Phone res", .OpiEnv$DayDream$width, .OpiEnv$DayDream$height, .OpiEnv$DayDream$single_width, .OpiEnv$DayDream$single_height))
    return(NULL)
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
    if (is.null(stim)) return(list(err = "no stimulus"))
    
    if (is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$size)) return(list(err="No size in stimulus", seen=NA, time=NA))
    if (is.null(stim$level)) return(list(err="No level in stimulus", seen=NA, time=NA))
    if (is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
    if (is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))
    if (is.null(stim$eye)) return(list(err="No eye in stimulus", seen=NA, time=NA))
    # if no info about stimulus color, then it is white
    if(is.null(stim$color)) stim$color <- "white"
    
    # make the stimulus
    bg <- ifelse(stim$eye == "L", .OpiEnv$DayDream$background_left, .OpiEnv$DayDream$background_right)
    len <- 51               # length of the image forced to be 51x51
    radius <- (len - 1) / 2 # and radius too
    npix <- len^2           # get number of pixels
    im <- matrix(bg, 3, npix)
    
    x <- 0:(npix - 1) %% len
    y <- 0:(npix - 1) %/% len
    if(stim$color == "red")        idx <- 1
    else if(stim$color == "green") idx <- 2
    else if(stim$color == "blue")  idx <- 3
    else                           idx <- 1:3
    im[idx,(x - radius)^2 + (y - radius)^2 < radius^2] <- find_pixel_value(stim$level)
    if (load_image(im, len, len)) {
        msg <- paste("OPI_MONO_PRESENT", stim$eye, stim$x, stim$y, stim$size, stim$duration, stim$responseWindow, sep=" ")
        writeLines(msg, .OpiEnv$DayDream$socket)
        
        seen <- readBin(.OpiEnv$DayDream$socket, "integer", size=1)
        time <- readBin(.OpiEnv$DayDream$socket, "double", size=4, endian=.OpiEnv$DayDream$endian)
        readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n
        # seen or not seen? if 1, then seen is TRUE, otherwise is FALSE
        seen <- seen == "1"
        
        if (!seen && time == 0)
            return(list(err="Background image not set", seen=NA, time=NA))
        if (!seen && time == 1)
            return(list(err="Trouble with stim image", seen=NA, time=NA))
        if (!seen && time == 2)
            return(list(err="Location out of range for daydream", seen=NA, time=NA))
        if (!seen && time == 3)
            return(list(err="OPI present error back from daydream", seen=NA, time=NA))
        
        return(list(
            err  = NULL,
            seen = seen,    # assumes 1 or 0, not "true" or "false"
            time = time
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

#' @rdname opiSetBackground
#' @param fix_cx fixation x position in degrees of visual angle. Default is 0
#' @param fix_cy fixation y position in degrees of visual angle. Default is 0
#' @param fix_sx fixation horizontal size in degrees of visual angle. Default is 1
#' @param fix_sy fixation vertical size in degrees of visual angle. Default is 1
#' @param fix_color fixation color
#' @param eye eye
#' @details
#' \subsection{Daydream}{
#'   \code{opiSetBackground(lum=10, color="white", fixation="Cross", fix_cx=0, fix_cy=0, fix_sx=2, fix_sy=2, fix_lum=10, fix_color="green", eye="L")}
#'   \itemize{
#'     \item{\code{lum}} background luminance in cd/\eqn{\mbox{m}^2}{m^2} is set to nearest grey
#'       value in \code{lut} from \code{opiInitialize}. Default is 10 cd/\eqn{\mbox{m}^2}{m^2}
#'     \item{\code{color}} color of the background. It can be \code{'white'} (default) or
#'       \code{'green'}.
#'     \item{\code{fixation}} can only be \code{'Cross'} at the moment.
#'     \item{\code{fix_cx}, \code{fix_cy}} fixation (x, y) position in degrees
#'       of visual angle
#'     \item{\code{fix_sx}, \code{fix_sy}} dimensions of fixation target in
#'       degrees of visual angle
#'     \item{\code{fix_lum}} luminance of the fixation target in cd/\eqn{\mbox{m}^2}{m^2} is set to
#'       nearest grey value in \code{lut} from \code{opiInitialize}. Default
#'       is 15 cd/\eqn{\mbox{m}^2}{m^2}
#'     \item{\code{fix_color}} color of the fixation target. It can be \code{'white'} or
#'       \code{'green'}  (default).
#'   }
#' }
#' @return
#' \subsection{Daydream}{ 
#'   DETAILS
#' }
daydream.opiSetBackground <- function(eye, lum=10, color="white", fixation="Cross",
                                      fix_cx=0, fix_cy=0, fix_sx=2, fix_sy=2,
                                      fix_color="green") {
    if (is.na(lum)) {
        warning('Cannot set background to NA in opiSetBackground')
        return('Cannot set background to NA in opiSetBackground')
    }
    if (is.na(color)) {
      color = "white"
      warning('Background color set to \'white\' by default')
    }
    # get pixel value of background from luminance in cd/m2
    bg <- find_pixel_value(lum)
    writeLines(paste("OPI_MONO_SET_BG", eye, bg), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res != "OK")
        return(paste0("Cannot set background to ",bg," in opiSetBackground"))
    # add background information
    .OpiEnv$DayDream$background_color   <- color
    .OpiEnv$DayDream$fix_color          <- fix_color
    if (eye == "L") {
      .OpiEnv$DayDream$background_left  <- bg
      .OpiEnv$DayDream$background_right <- 0
    } else {
      .OpiEnv$DayDream$background_left  <- 0
      .OpiEnv$DayDream$background_right <- bg
    }   
    # if even, add 1 to fixation size
    imsize <- 51
    m <- (imsize + 1) / 2
    # design fixation target
    npix <- imsize^2
    im <- matrix(bg, 3, npix)
    x <- 0:npix %% imsize + 1
    y <- 0:npix %/% imsize + 1
    # choose color
    if(fix_color == "white")     col <- c(255, 255, 255)
    else if(fix_color == "red")  col <- c(255, 0, 0)
    else if(fix_color == "blue") col <- c(0, 0, 255)
    else                         col <- c(0, 255, 0)
    if(fixation == "Cross") {
      # thickness of 5 lines
      idx <- which((x >= m - 2 & x <= m + 2) | (y >= m - 2 & y <= m + 2))
    } else if(fixation == "Circle") {
      r   <- (imsize - 1) / 2
      # thickness of 5 lines
      idx <- which((x - m)^2 + (y - m)^2 < r^2 & (x - m)^2 + (y - m)^2 >= (r - 5)^2)
    }
    im[,idx] <- col
    if (!load_image(im, imsize, imsize))
      return("Trouble loading fixation image in opiSetBackground.")
    writeLines(paste("OPI_MONO_BG_ADD", eye, fix_cx, fix_cy, fix_sx, fix_sy), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res != "OK")
      return("Trouble adding fixation to background in opiSetBackground")
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
        return(NULL)
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
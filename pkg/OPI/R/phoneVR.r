# OPI for phoneVR
# 
# Author: Ivan Marin-Frach
# Date: Mar 2022 
#
# Copyright 2019 Andrew Turpin (aturpin@unimelb.edu.au)
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

if (exists(".OpiEnv") && !exists("PhoneVR", where = .OpiEnv)) {
  .OpiEnv$PhoneVR <- new.env(25)
  # connection
  .OpiEnv$PhoneVR$socket <- NA
  # function that define and reset all phoneVR parameters
  resetAllPhoneVRParameters <- function() {
    # properties of PhoneVR
    .OpiEnv$PhoneVR$width     <- NA
    .OpiEnv$PhoneVR$height    <- NA
    .OpiEnv$PhoneVR$xdpi      <- NA
    .OpiEnv$PhoneVR$ydpi      <- NA
    .OpiEnv$PhoneVR$xlim      <- NA
    .OpiEnv$PhoneVR$ylim      <- NA
    .OpiEnv$PhoneVR$roomlight <- NA
    # look up table
    .OpiEnv$PhoneVR$lut       <- NA
    # background data: background eye (R, L, or B), luminance in cdm2 and color
    .OpiEnv$PhoneVR$bgeye     <- NA
    .OpiEnv$PhoneVR$bglum     <- NA
    .OpiEnv$PhoneVR$bgcol     <- NA
    # fixation data: type of fixation, eye where fixation is drawn (R, L, or B),
    # luminance in cdm2, color, fixation center (x and y) and size (x and y) in degrees
    .OpiEnv$PhoneVR$fixeye    <- NA
    .OpiEnv$PhoneVR$fixtype   <- NA
    .OpiEnv$PhoneVR$fixlum    <- NA
    .OpiEnv$PhoneVR$fixcol    <- NA
    .OpiEnv$PhoneVR$fixcx     <- NA
    .OpiEnv$PhoneVR$fixcy     <- NA
    .OpiEnv$PhoneVR$fixsx     <- NA
    .OpiEnv$PhoneVR$fixsy     <- NA
  }
  # define and all phoneVR parameters
  resetAllPhoneVRParameters()
}

# It is 8 bit depth, so it must return a value from 0 to 1
phoneVR_find_lum_value <- function(cdm2)
  return((which.min(abs(.OpiEnv$PhoneVR$lut - cdm2)) - 1) / (length(.OpiEnv$PhoneVR$lut) - 1))

# It is 8 bit depth, so it must return a value from 0 to 1
phoneVR_parse_color_string <- function(col)
  return(tryCatch(t(col2rgb(col, alpha = 1)) / 255, error = function(e) NA))

#' @rdname opiInitialize
#' @param ip IP address on which server is listening for PhoneVR
#' @param port Port number on which server is listening for PhoneVR. Default is 50008
#' @param lut Look up table mapping pixel values to cd/m2
#' @details
#' \subsection{PhoneVR}{
#'   \code{opiInitialize(serverPort, port = 50008, lut = seq(0, 400, length.out = 256))}
#'   If the chosen OPI implementation is \code{PhoneVR}, then you must specify
#'   the IP address of the Android phoneVR that is in the PhoneVR, and the port on
#'   which the server running on the phoneVR is listening.
#'   \itemize{
#'     \item\code{ip} is the IP address of the PhoneVR server as a string
#'     \item\code{port} is the TCP/IP port of the PhoneVR server as a number
#'     \item\code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the
#'       cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is
#'       \code{seq(0, 4000, length.out = 256)}
#'   }
#' }
#' @return
#' \subsection{PhoneVR}{
#'   Returns NULL if connection is made, otherwise, it returns a text with the error
#' }
phoneVR.opiInitialize <- function(ip, port = 50008, lut = seq(0, 400, length.out = 256)) {
  # if connection is open, then don't open a new one
  if(!is.na(.OpiEnv$PhoneVR$socket)) opiClose()
  print(paste0("Looking for phone at ", ip, ":", port))
  socket <- tryCatch(
    socketConnection(host = ip, port, open = "w+b", blocking = TRUE, timeout = 5),
    warning = function(w) return(NULL),
    error   = function(e) return(NULL))
  if(is.null(socket)) return(paste("Cannot connect to phone at", ip, "and port", port))
  .OpiEnv$PhoneVR$socket <- socket
  resetAllPhoneVRParameters()
  # set look-up table
  .OpiEnv$PhoneVR$lut <- lut
  # get phone metrics
  writeLines("OPI_GET_METRICS", .OpiEnv$PhoneVR$socket)
  .OpiEnv$PhoneVR$width  <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  .OpiEnv$PhoneVR$height <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  .OpiEnv$PhoneVR$xdpi   <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  .OpiEnv$PhoneVR$ydpi   <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  .OpiEnv$PhoneVR$xlim   <- c(-round(as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1)), 1),
                             round(as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1)), 1))
  .OpiEnv$PhoneVR$ylim   <- c(-round(as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1)), 1),
                             round(as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1)), 1))
  .OpiEnv$PhoneVR$roomlight <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  print(paste("PhoneVR resolution in pixels:", .OpiEnv$PhoneVR$width, .OpiEnv$PhoneVR$height))
  print(paste0("PhoneVR x limits: (", .OpiEnv$PhoneVR$xlim[1], ", ", .OpiEnv$PhoneVR$xlim[2], ") degs"))
  print(paste0("PhoneVR y limits: (", .OpiEnv$PhoneVR$ylim[1], ", ", .OpiEnv$PhoneVR$ylim[2], ") degs"))
  print(paste0("Pixel density (x, y): (", .OpiEnv$PhoneVR$xdpi, ", ", .OpiEnv$PhoneVR$ydpi, ") dpi"))
  print(paste("Ambient luminance is:", .OpiEnv$PhoneVR$roomlight, "cdm2"))
  return(NULL)
}

#' @rdname opiSetBackground
#' @param bgeye eye where to display the background, can be left, right, or both
#' @param bglum background luminance
#' @param bgcol background color. Either a color name that R can understand
#'   or a vector with R, G, B, and alpha channels
#' @param fixeye eye for the fixation target, can be left, right, or both
#' @param fixtype the target of the fixation
#' @param fixcx fixation x position in degrees of visual angle. Default is 0
#' @param fixcy fixation y position in degrees of visual angle. Default is 0
#' @param fixsx fixation horizontal size in degrees of visual angle. Default is 1
#' @param fixsy fixation vertical size in degrees of visual angle. Default is 1
#' @param fixtheta angle of rotation of the fixation targert in degrees. Default is 0
#' @param fixlum fixation luminance
#' @param fixcol fixation color. Same specifications as background color
#' @details
#' \subsection{PhoneVR}{
#'   \code{opiSetBackground(bgeye, bglum = 10, bgcol = "white", fixeye = eye, fixtype = "Cross", fixlum = 100, fixcol = "green", fixcx = 0, fixcy = 0, fixsx = 2, fix_sy = 2)}
#'   \itemize{
#'     \item{\code{bgeye}} eye for the background. Can be "R", "L", or "B" for right, left, or both
#'     \item{\code{bglum}} background luminance in cd/\eqn{\mbox{m}^2}{m^2} is set to nearest grey
#'       value in \code{lut} from \code{opiInitialize}. Default is 10 cd/\eqn{\mbox{m}^2}{m^2}
#'     \item{\code{bgcol}} color of the background. Default is white
#'     \item{\code{fixeye}} eye for the background. Takes the same values as \code{eye} (default)
#'     \item{\code{fixtype}} fixation target
#'     \item{\code{fixcx}, \code{fixcy}} fixation (x, y) position in degrees of visual angle
#'     \item{\code{fixsx}, \code{fixsy}} dimensions of fixation target in degrees of visual angle
#'     \item{\code{fixtheta}} angle of rotation of the fixation target
#'     \item{\code{fixlum}} luminance of the fixation target. Default is 100 cd/\eqn{\mbox{m}^2}{m^2}
#'     \item{\code{fixcol}} color of the fixation target. Default is green
#'   }
#' }
#' @return
#' \subsection{PhoneVR}{ 
#'   Sets background for left, right, or both eyes in phoneVR
#' }
phoneVR.opiSetBackground <- function(bgeye, bglum = 10, bgcol = "white",
                                     fixeye = bgeye, fixtype = "none",
                                     fixcx = 0, fixcy = 0, fixsx = 2, fixsy = 2, fixtheta = 0,
                                     fixlum = 100, fixcol = "green") {
  # parse eyes
  bgeyenum  <- switch(bgeye, "L" = 0, "R" = 1, "B" = 2, NA)
  fixeyenum <- switch(fixeye, "L" = 0, "R" = 1, "B" = 2,  NA)
  if(is.na(bgeyenum)) stop("Wrong eye code")
  if(is.na(fixeyenum)) stop("Wrong eye code")
  # parse luminance
  bglum  <- phoneVR_find_lum_value(bglum)
  fixlum <- phoneVR_find_lum_value(fixlum)
  # parse color
  bgcol <- phoneVR_parse_color_string(bgcol)
  if(any(is.na(bgcol))) stop("Wrong color format for background color")
  fixcol <- phoneVR_parse_color_string(fixcol)
  if(any(is.na(fixcol))) stop("Wrong color format for fixation color")
  # if all good, then send background
  msg <- paste("OPI_SET_BACKGROUND", bgeyenum, bglum, bgcol[1], bgcol[2], bgcol[3], bgcol[4],
                                     fixeyenum, fixtype, fixcx, fixcy, fixsx, fixsy, fixtheta,
                                     fixlum, fixcol[1], fixcol[2], fixcol[3], fixcol[4])
  writeLines(msg, .OpiEnv$PhoneVR$socket)
  msg <- readLines(.OpiEnv$PhoneVR$socket, n = 1)
  if(msg != "OK") return(msg)
  .OpiEnv$PhoneVR$bgeye   <- bgeye
  .OpiEnv$PhoneVR$bglum   <- bglum
  .OpiEnv$PhoneVR$bgcol   <- bgcol
  .OpiEnv$PhoneVR$fixeye  <- fixeye
  .OpiEnv$PhoneVR$fixtype <- fixtype
  .OpiEnv$PhoneVR$fixlum  <- fixlum
  .OpiEnv$PhoneVR$fixcol  <- fixcol
  .OpiEnv$PhoneVR$fixcx   <- fixcx
  .OpiEnv$PhoneVR$fixcy   <- fixcy
  .OpiEnv$PhoneVR$fixsx   <- fixsx
  .OpiEnv$PhoneVR$fixsy   <- fixsy
  return(NULL)
}

#' @rdname opiPresent
#' @details
#' \subsection{PhoneVR}{
#'   If the chosen OPI implementation is \code{PhoneVR}, then \code{nextStim}
#'   is ignored. PhonVR
#' }
phoneVR.opiPresent <- function(stim, nextStim = NULL) {
  # check integrity
  if(is.null(stim))       return(list(err = "no stimulus"))
  if(is.null(stim$eye))   return(list(err = "No eye in stimulus",            seen = NA, time = NA))
  if(is.null(stim$cx))    return(list(err = "No x coordinate in stimulus",   seen = NA, time = NA))
  if(is.null(stim$cy))    return(list(err = "No y coordinate in stimulus",   seen = NA, time = NA))
  if(is.null(stim$sx))    return(list(err = "No width in stimulus",          seen = NA, time = NA))
  if(is.null(stim$lum))   return(list(err = "No lum in stimulus",            seen = NA, time = NA))
  # fill defaults
  if(is.null(stim$nsteps)) stim$nsteps <- 1        # defaults to 1 step in the presentation
  if(is.null(stim$d))      stim$d      <- 200      # defaults to 200 ms presentation
  if(is.null(stim$w))      stim$w      <- 1000     # defaults to a response window of a second
  if(is.null(stim$sy))     stim$sy     <- stim$sx  # defaults to the the same as size in x
  if(is.null(stim$type))   stim$type   <- "circle" # defaults to circle
  if(is.null(stim$theta))  stim$theta  <- 0        # angle of rotation defaults to zero
  if(is.null(stim$col))    stim$col    <- "white"  # defaults to white
  if(is.null(stim$tstep))  stim$tstep  <- floor(stim$d / stim$nsteps)
  if(stim$nsteps > 1 & length(stim$eye)    == 1) stim$eye    <- rep(stim$eye,    stim$nsteps)
  if(stim$nsteps > 1 & length(stim$type)   == 1) stim$type   <- rep(stim$type,   stim$nsteps)
  if(stim$nsteps > 1 & length(stim$cx)     == 1) stim$cx     <- rep(stim$cx,     stim$nsteps)
  if(stim$nsteps > 1 & length(stim$cy)     == 1) stim$cy     <- rep(stim$cy,     stim$nsteps)
  if(stim$nsteps > 1 & length(stim$sx)     == 1) stim$sx     <- rep(stim$sx,     stim$nsteps)
  if(stim$nsteps > 1 & length(stim$sy)     == 1) stim$sy     <- rep(stim$sy,     stim$nsteps)
  if(stim$nsteps > 1 & length(stim$sy)     == 1) stim$sy     <- rep(stim$sy,     stim$nsteps)
  if(stim$nsteps > 1 & length(stim$theta)  == 1) stim$theta  <- rep(stim$theta,  stim$nsteps)
  if(stim$nsteps > 1 & length(stim$tstep)  == 1) stim$tstep  <- rep(stim$tstep,  stim$nsteps)
  if(stim$nsteps > 1 & length(stim$lum)    == 1) stim$lum    <- rep(stim$lum,    stim$nsteps)
  if(stim$nsteps > 1 & length(stim$col)    == 1) stim$col    <- rep(stim$col,    stim$nsteps)
  # check consistency
  if(length(stim$type)  != stim$nsteps |
     length(stim$cx)    != stim$nsteps | length(stim$cy)  != stim$nsteps |
     length(stim$sx)    != stim$nsteps | length(stim$sy)  != stim$nsteps |
     length(stim$theta) != stim$nsteps | length(stim$lum) != stim$nsteps |
     length(stim$tstep) != stim$nsteps | length(stim$col) != stim$nsteps)
    stop("stimulus parameters inconsistent")
  # parse data and get luminance from the Gamma look-up-table
  stim$eye <- sapply(stim$eye, switch, "L" = 0, "R" = 1, "B" = 2, NA)
  if(any(is.na(stim$eye))) stop("Wrong eye code")
  # get stimulus luminance
  stim$lum <- sapply(stim$lum, phoneVR_find_lum_value)
  # parse color
  stim$col <- phoneVR_parse_color_string(stim$col)
  if(any(is.na(stim$col))) stop("Wrong color format for stimulus color")
  # send global parameters of the stimulus to present:
  # number of steps, presentation time and response window
  msg <- paste("OPI_PRESENT", stim$nsteps, stim$d, stim$w)
  writeLines(msg, .OpiEnv$PhoneVR$socket)
  # check if received OK
  msg <- readLines(.OpiEnv$PhoneVR$socket, n = 1)
  if(msg != "OK") return(list(err = msg, seen = NA, time = NA))
  # send each stimulus step to the OPI server
  for(i in 1:stim$nsteps) {
    msg <- paste(stim$eye[i], stim$type[i],
                 stim$cx[i], stim$cy[i], stim$sx[i], stim$sy[i], stim$theta[i],
                 stim$tstep[i], stim$lum[i],
                 stim$col[i,1], stim$col[i,2], stim$col[i,3], stim$col[i,4])
    writeLines(msg, .OpiEnv$PhoneVR$socket)
    # check if received OK
    msg <- readLines(.OpiEnv$PhoneVR$socket, n = 1)
    if(msg != "OK") return(list(err = msg, seen = NA, time = NA))
  }
  # at this point, the server presents the stimulus and returns results,
  # including possible errors during presentation
  err <- readLines(.OpiEnv$PhoneVR$socket, n = 1)
  if(err == "") err <- NULL
  seen <- as.logical(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  time <- as.numeric(readLines(.OpiEnv$PhoneVR$socket, n = 1))
  return(list(err = err, seen = seen, time = time))
}

#' @rdname opiClose
#' @return
#' \subsection{PhoneVR}{
#'   Closes the socket connection with the PhoneVR
#' }
phoneVR.opiClose <- function() {
  if(is.na(.OpiEnv$PhoneVR$socket)) return(NULL)
  writeLines("OPI_CLOSE", .OpiEnv$PhoneVR$socket)
  msg <- readLines(.OpiEnv$PhoneVR$socket, n = 1)
  close(.OpiEnv$PhoneVR$socket)
  .OpiEnv$PhoneVR$socket <- NA
  resetAllPhoneVRParameters()
  if(length(msg) == 0 || msg == "OK") return(NULL)
  else return(msg)
}

#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @details
#' \subsection{PhoneVR}{
#'   Returns all constants in \code{.OpiEnv$PhoneVR} as a list.
#' }
#' \subsection{PhoneVR}{
#'   Returns all constants in \code{.OpiEnv$PhoneVR} as a list.
#' }
phoneVR.opiQueryDevice <- function() {
  vars <- ls(.OpiEnv$PhoneVR)
  lst <- lapply(vars, function(i) .OpiEnv$PhoneVR[[i]])
  names(lst) <- vars
  return(lst)
}
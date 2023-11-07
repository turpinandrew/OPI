#
# OPI for imo
# 
# Based on imoClient.r.
# 
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: February 2016
#
# Copyright 2016 Andrew Turpin
#
# Contributor: Ziying Yang (ziying.yang@unimelb.edu.au)
# Date: November 2019
#
# Copyright [2016] [Andrew Turpin]
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
# .OpiEnv$Imo$socket is the connection to the imo
# .OpiEnv$Imo$...    constants and setting variables
###################################################################
if (exists(".OpiEnv") && !exists("Imo", where=.OpiEnv)) {
    assign("Imo", new.env(25), envir=.OpiEnv)
  # socket connection
  .OpiEnv$Imo$socket <- NA
  # screen dimensions and pixel size
  .OpiEnv$Imo$ppd    <- NA
  .OpiEnv$Imo$width  <- NA
  .OpiEnv$Imo$height <- NA
  # tracking ON or OFF (as in the Compass).
  # IMO will always return pupil position
  # IMF NEEDS TO TALK TO TURPIN TO SEE HOW WE HANDLE THIS
  .OpiEnv$Imo$tracking <- NA
  .OpiEnv$Imo$tracktol <- NA
  # constants
  .OpiEnv$Imo$SEEN     <- 1  
  .OpiEnv$Imo$NOT_SEEN <- 0
  # background and target values
  .OpiEnv$Imo$bgl   <- NA # left eye background luminance in dB
  .OpiEnv$Imo$tgl   <- NA # left eye fixation target
  .OpiEnv$Imo$tgldb <- NA # left eye fixation target luminance in dB
  .OpiEnv$Imo$tglx  <- NA # left eye fixation target x-position in degrees
  .OpiEnv$Imo$tgly  <- NA # left eye fixation target y-position in degrees
  .OpiEnv$Imo$bgr   <- NA # right eye background luminance in dB
  .OpiEnv$Imo$tgr   <- NA # right eye target type
  .OpiEnv$Imo$tgrdb <- NA # right eye target luminance in dB
  .OpiEnv$Imo$tgrx  <- NA # right eye fixation target x-position in degrees
  .OpiEnv$Imo$tgry  <- NA # right eye fixation target y-position in degrees
}

# check if a command returned OK signal
checkOK <- function(txt, stop_if_bad = TRUE) {
  res <- readChar(.OpiEnv$Imo$socket, nchars = 1)
  if (res != "0") {
    if (stop_if_bad) stop(paste(txt, "did not return OK from imo"))
    else warning(paste(txt, "did not return OK from imo"))
  }
}

# send parameters of a command (for, e.g., set commands)
send_FCN <- function(i) {
  s <- as.character(i)
  if (nchar(s) > 8) stop(paste('send_FCN error in imo:', i, 'too large'))
  s <- paste0(nchar(s), s)
  writeChar(s, .OpiEnv$Imo$socket, nchars = nchar(s), eos = NULL)
}

# retrieve info from server (for, e.g., get commands)
read_FCN <- function() {
  n <- as.numeric(readChar(.OpiEnv$Imo$socket, nchars = 1))
  if (n > 8) stop(paste('read_FCN error in imo:', n, 'too large'))
  s <- readChar(.OpiEnv$Imo$socket, nchars = n)
  return(as.numeric(s))
}

# generate circular stimulus
circularStimulus <- function(rad, bg, fg) {
  diam <- 2 * rad + 1
  im <- matrix(.OpiEnv$Imo$bgr, diam, diam)
  xx <- matrix(rep(-rad:rad, diam), diam, diam, byrow = TRUE)
  yy <- matrix(rep(-rad:rad, diam), diam, diam)
  im[xx^2 + yy^2 < rad^2] <- fg
  return(im)
}

# Load an image to server
load_images <- function(imlist) {
  for(i in 1:length(imlist)){
    # send LOAD command
    writeChar("LOAD", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
    # send the dimensions of the images to send
    send_FCN(dim(imlist[[i]]$R)[1])
    send_FCN(dim(imlist[[i]]$R)[2])
    send_FCN(dim(imlist[[i]]$L)[1])
    send_FCN(dim(imlist[[i]]$L)[2])
    # send the images themselves
    writeBin(as.integer(c(imlist[[i]]$R)), .OpiEnv$Imo$socket, size = 1)
    writeBin(as.integer(c(imlist[[i]]$L)), .OpiEnv$Imo$socket, size = 1)
    # check if upload went well
    checkOK("LOAD", stop_if_bad = TRUE)
  }
  print(" LOADING finish :)")
}

present_stim <- function(n, xy, eye, d, r, cycle) {
  writeChar("PRES", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
  
  send_FCN(n)
  
  for(i in 1:n){
    send_FCN(i) # image number i
    # send coordinates in pixel and from fixation target
    send_FCN(xy[[i]]$R[1])
    send_FCN(xy[[i]]$R[2])
    send_FCN(xy[[i]]$L[1])
    send_FCN(xy[[i]]$L[2])
    # eye where to present
    send_FCN(as.integer(eye[i]))
    # stimulus duration
    send_FCN(as.integer(d[i]))
  }
  # cycle: check protocols
  send_FCN(cycle)
  # wait time
  send_FCN(as.integer(r))
  
  # tracking on always, no reason not to
  writeChar("1", .OpiEnv$Imo$socket, nchars = 1, eos = NULL)
  # check that presentation went well
  checkOK("PRES", stop_if_bad = TRUE)
}

# get pupil info
getPupilInfo <- function() {
  n <- read_FCN() # number of pupils
  pupil <- ellipse <- times <- NULL
  for (i in 1:n) {
    d <- readChar(.OpiEnv$Imo$socket, nchars = 1)
    if (d == "1") {
      pupil   <- c(pupil  , list(list(dx=read_FCN(), dy=read_FCN())))
      ellipse <- c(ellipse, list(list(d1=read_FCN(), d2=read_FCN(), rho=read_FCN())))
      times   <- c(times  , read_FCN())
    } else {
      pupil   <- c(pupil  , list(list(dx=NA, dy=NA)))
      ellipse <- c(ellipse, list(list(d1=NA, d2=NA, rho=NA)))
      times   <- c(times  , NA)
    }
  }
  return(NULL)
}

# Bresenham's Integer Line Drawing Algorithm
# download from https://rdrr.io/github/ornelles/EBImageExtra/man/bresenham.html
# on 27/11/2019
#
# Generate integer x,y points between vertices by Bresenham's algorithm. 
#
# @param x,y the x and y coordinates a points to be joined where y can
#  can be missing and the argument x is processed by \code{\link{grDevices::xy.coords}}.
# @param close \code{logical} value indicating if the points form a closed
#  polygon (without duplicating the first and last points)
#
# @return
#
# A list of length 2 with \code{integer} \code{x,y} coordinates connecting
#   the vertices
# @examples
# # simple line
#   bresenham(x = c(1, 4), y = c(1, 12))
# # closed and open polygon
#   verts <- list(x = c(1, 9, 6, 4, 2), y = c(9, 9, 2, 3, 1))
#   plot(verts, type = "l", ylim = c(0, 10), xlim = c(0, 10))
#   points(bresenham(verts)) # closed
#   points(bresenham(verts, close = FALSE), col = 2, pch = 16)
bresenham <- function(x, y = NULL, close = TRUE) {
  # accept any coordinate structure
  v <- grDevices::xy.coords(x = x, y = y, recycle = TRUE, setLab = FALSE)
  if (!all(is.finite(v$x), is.finite(v$y)))
    stop("finite coordinates required")
  
  v[1:2] <- lapply(v[1:2], round) # Bresenham's algorithm IS for integers
  nx <- length(v$x)
  if (nx == 1) return(list(x = v$x, y = v$y)) # just one point
  if (nx > 2 && close == TRUE) { # close polygon by replicating 1st point
    v$x <- c(v$x, v$x[1])
    v$y <- c(v$y, v$y[1])
    nx <- nx + 1
  }
  # collect result in 'ans, staring with 1st point
  ans <- lapply(v[1:2], "[", 1)
  
  # process all vertices in pairs
  for (i in seq.int(nx - 1)) {
    x <- v$x[i] # coordinates updated in x, y
    y <- v$y[i]
    x.end <- v$x[i + 1]
    y.end <- v$y[i + 1]
    
    dx <- abs(x.end - x); dy <- -abs(y.end - y)
    sx <- ifelse(x < x.end, 1, -1)
    sy <- ifelse(y < y.end, 1, -1)
    err <- dx + dy
    
    # process one segment
    while(!(isTRUE(all.equal(x, x.end)) && isTRUE(all.equal(y, y.end)))) {
      e2 <- 2 * err
      if (e2 >= dy) { # increment x
        err <- err + dy
        x <- x + sx
      }
      if (e2 <= dx) { # increment y
        err <- err + dx
        y <- y + sy
      }
      ans$x <- c(ans$x, x)
      ans$y <- c(ans$y, y)
    }
  }
  # remove duplicated points (typically 1st and last)
  dups <- duplicated(do.call(cbind, ans), MARGIN = 1) 
  return(lapply(ans, "[", !dups))
}

#######################################################################
# INPUT: 
#   ip       = ip address on which server is listening
#   port     = port number on which server is listening
#   ppd      = pixels size, i.e., pixels per degree
#   tracking = whether to use tracking during stimulus presentation
#   tracktol = tolerance for tracking in degrees
#
# @return NULL if succeed
# @return 1 server not found/ready at the ip+port provided
#######################################################################
#' @rdname opiInitialize
#' @param ppd pixels size as in pixels per degree
#' @param tracking tracking on or off
#' @param tracktol tolerance during tracking in degrees of visual angle
#' @details
#' # imo
#'   \code{opiInitialize(ip, port, ppd = 16, tracking = FALSE, tracktol = 2)}
#'   
#'   If the chosen OPI implementation is \code{imo}, then you must specify the IP
#'   address and port of the imo server.
#'   
#'     * \code{ip} is the IP address of the imo server as a string.
#'     * \code{port} is the TCP/IP port of the imo server as a number.
#'     * \code{ppd} Pixel size in pixels per degree. Default is 16 ppd.
#'     * \code{tracking} Whether to use tracking during stimulus presentation. Default is FALSE.
#'     * \code{tracktol} Tolerance during tracking in degrees of visual angle.  The system does not show any stimulus if eye is not within \code{tracktol} degrees of visual angle from fixation point. Default is 2 degrees.
#' 
#' @return
#' ## imo
#'   Always returns NULL. Will \code{stop} if there is an error.
#' 
#' @examples
#' \dontrun{
#'   # Set up the imo
#'   chooseOpi("imo")
#'   opiInitialize(ip = "192.168.1.7", port = 1234)
#' }
imo.opiInitialize <- function(ip = "localhost", port = 1234, ppd = 16,
                              tracking = FALSE, tracktol = 2) {
  cat("Looking for server... ")

  socket <- tryCatch(
      socketConnection(host = ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
      error = function(e) stop(paste("Cannot connect to IMO at", ip, "on port", port))
  )

  print("found server :)")
  assign("socket", socket, envir = .OpiEnv$Imo)
  writeChar("STAR", socket, nchars = 4, eos = NULL)
  checkOK("STAR", stop_if_bad = TRUE)

  # store the pixels per degree
  .OpiEnv$Imo$ppd      <- ppd
  .OpiEnv$Imo$tracking <- tracking
  .OpiEnv$Imo$tracktol <- tracktol
  # get screen dimensions
  writeChar("GETD", socket, nchars = 4, eos = NULL)
  .OpiEnv$Imo$width  <- read_FCN()
  .OpiEnv$Imo$height <- read_FCN()
  checkOK("GETD", stop_if_bad = TRUE)
  # set default background
  opiSetBackground()

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
imo.opiPresent <- function(stim, nextStim = NULL) { UseMethod("imo.opiPresent") }
setGeneric("imo.opiPresent")

imo.opiPresent.opiStaticStimulus <- function(stim, ...) {
  if (is.null(stim)) stop("opiPresent: NULL stimulus")

  if(is.null(stim$x))     stop("opiPresent: no x value given in static stim")
  if(is.null(stim$y))     stop("opiPresent: no y value given in static stim")
  if(is.null(stim$level)) stop("opiPresent: no level value given in static stim")
  
  # defaults
  if(is.null(stim$size)) {
    warning("opiPresent: no stim size specified. Assuming Goldmann III = 26 / 60 degrees")
    stim$size <- 26 / 60
  }
  if(is.null(stim$eye)) {
    warning("opiPresent: no eye. Assuming stimulus is to be presented to both eyes")
    stim$eye <- "B"
  }
  if(is.null(stim$duration)) {
    warning("opiPresent: no stim duration specified. Assuming 200 ms")
    stim$duration <- 200
  }
  if(is.null(stim$responseWindow)) {
    warning("opiPresent: no stim responseWindow specified. Assuming 1500 ms")
    stim$responseWindow <- 1500
  }
  # convert coordinates from degrees to pixels
  xl <- round(.OpiEnv$Imo$ppd * (stim$x + .OpiEnv$Imo$tglx))
  yl <- round(.OpiEnv$Imo$ppd * (stim$y + .OpiEnv$Imo$tgly))
  xr <- round(.OpiEnv$Imo$ppd * (stim$x + .OpiEnv$Imo$tgrx))
  yr <- round(.OpiEnv$Imo$ppd * (stim$y + .OpiEnv$Imo$tgry))
  xy <- list(list(R = c(xr, yr), L = c(xl, yl)))
  # eye to present according to IMO protocol
  stim$eye <- switch(stim$eye, "R" = 0, "L" = 1, "B" = 2)
  if(is.null(stim$eye))
    stop("incorrect eye. It must be 'R' for right, 'L' for left or 'B' for both")
  # get stimulus radius in pixel
  rad <- round(.OpiEnv$Imo$ppd * stim$size / 2)
  # construct images. We need both because the background can be different in each eye,
  # even if the stimulus themselves are the same.
  imL <- circularStimulus(rad, .OpiEnv$Imo$bgl, stim$level)
  imR <- circularStimulus(rad, .OpiEnv$Imo$bgr, stim$level)
  # transfer the images and corresponding sizes
  # load images to the server
  load_images(list(list(R = imR, L = imL)))
  # present stimulus
  present_stim(1, xy, stim$eye, stim$duration, stim$responseWindow, 0)
  # read results
  p    <- readChar(.OpiEnv$Imo$socket, nchars = 1) # seen or not seen
  time <- read_FCN() # time if seen or -1 otherwise
  # get pupil tracking information
  pupilInfo <- getPupilInfo()
  # clear images
  writeChar("CLRL", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
  checkOK("CLRL", stop_if_bad = FALSE)
  # return results
  return(list(
    err     = NULL,
    seen    = p == "1",
    time    = time
    #pupil   = pupil, 
    #ellipse = ellipse, 
    #times   = times
  ))
}

########################################## 
# Not supported yet
########################################## 
imo.opiPresent.opiKineticStimulus <- function(stim, ...) {
  if (is.null(stim)) stop("opiPresent: NULL stimulus")
  
  if (is.null(stim$path))  stop("opiPresent: no path values given in kinetic stim")
  if (is.null(stim$size))  stop("opiPresent: no sizes values given in kinetic stim")
  if (is.null(stim$level)) stop("opiPresent: no levels values given in kinetic stim")
  if (is.null(stim$speed)) stop("opiPresent: no speeds values given in kinetic stim")
  # defaults
  if(is.null(stim$eye)) {
    warning("opiPresent: no eye. Assuming stimulus is to be presented to both eyes")
    stim$eye <- "B"
  }
  # if there is only 1 entry for stim$eye, then all segments are to be presented on
  # that eye
  if(length(stim$eye) == 1) stim$eye <- rep(stim$eye, length(stim$path$x) - 1)
  # all arrays in the list must have the same length or something is wrong
  if(length(stim$path$x) != length(stim$path$y)   ||
     length(stim$path$x) != length(stim$size) + 1 ||
     length(stim$size)   != length(stim$level)    ||
     length(stim$size)   != length(stim$speed)    ||
     length(stim$size)   != length(stim$eye))
    stop("incosistent stimulus definition")
  # eye to present according to IMO protocol
  stim$eye <- as.numeric(sapply(stim$eye, switch, "R" = 0, "L" = 1, "B" = 2))
  if(any(is.null(stim$eye)))
    stop("incorrect eyes. It must be 'R' for right, 'L' for left or 'B' for both")
  # convert stimulus radii in pixel
  rad  <- round(.OpiEnv$Imo$ppd * stim$size / 2)
  # convert to pixels from target fixation
  xl <- round(.OpiEnv$Imo$ppd * (stim$path$x + .OpiEnv$Imo$tglx))
  yl <- round(.OpiEnv$Imo$ppd * (stim$path$y + .OpiEnv$Imo$tgly))
  xr <- round(.OpiEnv$Imo$ppd * (stim$path$x + .OpiEnv$Imo$tgrx))
  yr <- round(.OpiEnv$Imo$ppd * (stim$path$y + .OpiEnv$Imo$tgry))
  # for each segment defined in stim$path, we need to find a continuous path
  # (as in all the pixels in the straightes line that connect the extremes of
  # the segments), generate the images for each pixel in the path, and get the
  # duration of presentation of each image based on the speed at that segment in
  # pixels per second, and the number of pixels in the path
  imlist <- NULL # init
  xy     <- NULL 
  eye    <- NULL
  d      <- NULL
  k      <- 0
  for(i in 1:(length(stim$path$x) - 1)) {
    # xy locations for segments
    xyl_seg <- bresenham(c(xl[i], xl[i + 1]), c(yl[i], yl[i + 1])) # left eye
    xyr_seg <- bresenham(c(xr[i], xr[i + 1]), c(yr[i], yr[i + 1])) # right eye
    # time of presentation of each image
    d_seg <- 1000 * sqrt(diff(xyl_seg$x)^2 + diff(xyl_seg$y)^2) / (.OpiEnv$Imo$ppd * stim$speed[i])
    # renove the last pixel
    xyl_seg$x <- xyl_seg$x[1:(length(xyl_seg$x) - 1)]
    xyl_seg$y <- xyl_seg$y[1:(length(xyl_seg$y) - 1)]
    xyr_seg$x <- xyr_seg$x[1:(length(xyr_seg$x) - 1)]
    xyr_seg$y <- xyr_seg$y[1:(length(xyr_seg$y) - 1)]
    # construct images. We need both because the background can be different in each eye,
    # even if the stimulus themselves are the same.
    imL <- circularStimulus(rad[i], .OpiEnv$Imo$bgl, stim$level[i])
    imR <- circularStimulus(rad[i], .OpiEnv$Imo$bgr, stim$level[i])
    for(j in 1:length(xyl_seg$x)) {
      k <- k + 1
      # positions
      xy[[k]] <- list(R = c(xyr_seg$x[j], xyr_seg$y[j]), L = c(xyl_seg$x[j], xyl_seg$y[j]))
      # image at each position
      imlist[[k]] <- list(R = imR, L = imL)
    }
    # eye to present
    eye <- c(eye, rep(stim$eye[i], length(xyl_seg$x)))
    # duration of presentation
    d   <- c(d, d_seg)
  }
  # load images
  load_images(imlist)
  # present stimulus
  present_stim(length(imlist), xy, eye, d, 0, 0)
  # read results
  p    <- readChar(.OpiEnv$Imo$socket, nchars = 1) # seen or not seen
  time <- read_FCN() # time if seen or -1 otherwise
  
  # get pupil tracking information
  pupilInfo <- getPupilInfo()
  
  # clear images
  writeChar("CLRL", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
  checkOK("CLRL", stop_if_bad = FALSE)
  
  return(list(
    err     = NULL,
    seen    = p == "1",
    time    = time
    #pupil   = pupil, 
    #ellipse = ellipse, 
    #times   = times
  ))
}

imo.opiPresent.opiTemporalStimulus <- function(stim, ...) {
  if (is.null(stim)) stop("opiPresent: NULL stimulus")
  # eye to present according to IMO protocol
  stim$eye <- as.numeric(sapply(stim$eye, switch, "R" = 0, "L" = 1, "B" = 2))
  if(any(is.null(stim$eye)))
    stop("incorrect eye. It must be 'R' for right, 'L' for left or 'B' for both")
  # convert coordinates from degrees to pixels
  for(i in 1:length(stim$imlist)) {
    stim$center[[i]]$R[1] <- round(.OpiEnv$Imo$ppd * (stim$center[[i]]$R[1] + .OpiEnv$Imo$tgrx))
    stim$center[[i]]$R[2] <- round(.OpiEnv$Imo$ppd * (stim$center[[i]]$R[2] + .OpiEnv$Imo$tgry))
    stim$center[[i]]$L[1] <- round(.OpiEnv$Imo$ppd * (stim$center[[i]]$L[1] + .OpiEnv$Imo$tglx))
    stim$center[[i]]$L[2] <- round(.OpiEnv$Imo$ppd * (stim$center[[i]]$L[2] + .OpiEnv$Imo$tgly))
  }
  
  # load images
  load_images(stim$imlist)
  # present stimulus
  present_stim(length(stim$imlist), stim$center, stim$eye,
               stim$duration, stim$responseWindow, stim$cycle)
  # read results
  p    <- readChar(.OpiEnv$Imo$socket, nchars = 1) # seen or not seen
  time <- read_FCN() # time if seen or -1 otherwise

  # get pupil tracking information
  pupilInfo <- getPupilInfo()

  # clear images
  writeChar("CLRL", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
  checkOK("CLRL", stop_if_bad = FALSE)

  return(list(
    err     = NULL,
    seen    = p == "1",
    time    = time
    #pupil   = pupil, 
    #ellipse = ellipse, 
    #times   = times
  ))
}

#' @rdname opiSetBackground
#' @param bgl   left eye background luminance in dB Default is 25
#' @param tgl   left eye fixation target. Default is 1
#' @param tgldb left eye fixation target luminance in dB. Default is 20
#' @param tglx  left eye fixation target x-position in degrees. Default is 0.
#' @param tgly  left eye fixation target y-position in degrees. Default is 0.
#' @param bgr   right eye background luminance in dB. Default is 25
#' @param tgr   right eye target type. Default is 1
#' @param tgrdb right eye target luminance in dB. Default is 20
#' @param tgrx  right eye fixation target x-position in degrees. Default is 0.
#' @param tgry  right eye fixation target y-position in degrees. Default is 0.
#' @details
#' \subsection{imo}{
#'   DETAILS
#' }
#' @return
#' \subsection{imo}{ 
#'   DETAILS
#' }
imo.opiSetBackground <- function(bgl = 30, tgl = 1, tgldb = 25, tglx = 0, tgly = 0, 
                                 bgr = 30, tgr = 1, tgrdb = 25, tgrx = 0, tgry = 0) {
  if(bgl < 0 | bgl > 50) stop("background luminance must between 0 and 50 dB")
  if(bgr < 0 | bgr > 50) stop("background luminance must between 0 and 50 dB")
  if(tgl < 0 | tgl > 5)  stop("target type must between 0 and 5")
  if(tgr < 0 | tgr > 5)  stop("target type must between 0 and 5")
  if(tgldb < 0 | tgldb > 50) stop("target luminance must between 0 and 50 dB")
  if(tgrdb < 0 | tgrdb > 50) stop("target luminance must between 0 and 50 dB")

  # send background values for right eye first and the left eye second
  writeChar("SETB", .OpiEnv$Imo$socket, nchars = 4, eos = NULL)
  # right eye
  send_FCN(bgr)
  send_FCN(tgr)
  send_FCN(tgrdb)
  send_FCN(round(.OpiEnv$Imo$ppd * tgrx))
  send_FCN(round(.OpiEnv$Imo$ppd * tgry))
  # left eye
  send_FCN(bgl)
  send_FCN(tgl)
  send_FCN(tgldb)
  send_FCN(round(.OpiEnv$Imo$ppd * tglx))
  send_FCN(round(.OpiEnv$Imo$ppd * tgly))
  # check transmition OK and if it went well, update background and target values
  checkOK("SETB")
  assign("bgl",   bgl,   envir = .OpiEnv$Imo)
  assign("tgl",   tgl,   envir = .OpiEnv$Imo)
  assign("tgldb", tgldb, envir = .OpiEnv$Imo)
  assign("tglx",  tglx,  envir = .OpiEnv$Imo)
  assign("tgly",  tgly,  envir = .OpiEnv$Imo)
  assign("bgr",   bgr,   envir = .OpiEnv$Imo)
  assign("tgr",   tgr,   envir = .OpiEnv$Imo)
  assign("tgrdb", tgrdb, envir = .OpiEnv$Imo)
  assign("tgrx",  tgrx,  envir = .OpiEnv$Imo)
  assign("tgry",  tgry,  envir = .OpiEnv$Imo)
  
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
    checkOK("opiClose")
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
    #cat("Defined constants and functions\n")
    #cat("-------------------------------\n")
    return(ls(envir=.OpiEnv$Imo))
}
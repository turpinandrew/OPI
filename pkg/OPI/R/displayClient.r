#
# OPI to display in a plot window.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: Dec 2019
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
# .OpiEnv$Display$width window width in pixels
# .OpiEnv$Display$height window height in pixels
# .OpiEnv$Display$pixsize viewing distance in cm
# .OpiEnv$Display$LUT has 256 entries. LUT[x] is cd/m^2 value for grey level x
###################################################################
if (exists(".OpiEnv") && !exists("Display", where=.OpiEnv)) {
  assign("Display", new.env(25), envir=.OpiEnv)
  # device management
  .OpiEnv$Display$oldDevice <- NA
  .OpiEnv$Display$devNumber <- NA
  # window properties
  .OpiEnv$Display$widthin  <- NA # in inches
  .OpiEnv$Display$heightin <- NA # in inches
  .OpiEnv$Display$widthpx  <- NA # in pixels
  .OpiEnv$Display$heightpx <- NA # in pixels
  .OpiEnv$Display$LUT      <- NA # look up table
  # x and y limits of the display in degrees
  .OpiEnv$Display$pixsize <- NA # ppi
  .OpiEnv$Display$xlimsc  <- NA
  .OpiEnv$Display$ylimsc  <- NA
  # x and y limits of the window in degrees
  .OpiEnv$Display$xlim    <- NA
  .OpiEnv$Display$ylim    <- NA
  # background data and defaults
  .OpiEnv$Display$background_lum   <- NA
  .OpiEnv$Display$background_color <- NA
  .OpiEnv$Display$fixation         <- NA
  .OpiEnv$Display$fix_cx           <- NA
  .OpiEnv$Display$fix_cy           <- NA
  .OpiEnv$Display$fix_sx           <- NA
  .OpiEnv$Display$fix_sy           <- NA
  .OpiEnv$Display$fix_color        <- NA
}

###########################################################################
# Find the closest pixel value (index into .OpiEnv$Display$LUT less 1)
# for cd/m^2 param cdm2
###########################################################################
# We force the screen to act as an 8-bit device, so it must return a value
# from 0 to 255
find_pixel_value_display <- function(cdm2)
  return(which.min(abs(.OpiEnv$Display$LUT - cdm2)) - 1)

drawBackground <- function() {
  # get pixel level
  bg <- find_pixel_value_display(.OpiEnv$Display$background_lum)
  
  if(.OpiEnv$Display$background_color == "black") {
    vals <- c(0, 0, 0)
  } else if (.OpiEnv$Display$background_color == "red") {
    vals <- bg * c(1, 0, 0)
  } else if (.OpiEnv$Display$background_color == "green") {
    vals <- bg * c(0, 1, 0)
  } else if (.OpiEnv$Display$background_color == "blue") {
    vals <- bg * c(0, 0, 1)
  } else vals <- bg * c(1, 1, 1) # anything else, means white
  bg <- grDevices::rgb(vals[1], vals[2], vals[3], maxColorValue = 255)
  
  p <- par("usr")
  rect(p[1], p[3], p[2], p[4], border = NA, col = bg)
  if(.OpiEnv$Display$fixation == "Cross") {
    lines(.OpiEnv$Display$fix_cx + c(-.OpiEnv$Display$fix_sx, .OpiEnv$Display$fix_sx) / 2,
          c(.OpiEnv$Display$fix_cy, .OpiEnv$Display$fix_cy),
          lwd = 3, col = .OpiEnv$Display$fix_color)
    lines(c(.OpiEnv$Display$fix_cx,.OpiEnv$Display$fix_cx),
          .OpiEnv$Display$fix_cy + c(-.OpiEnv$Display$fix_sy, .OpiEnv$Display$fix_sy) / 2,
          lwd = 3, col = .OpiEnv$Display$fix_color)
  } else if (.OpiEnv$Display$fixation == "Circle") {
    symbols(.OpiEnv$Display$fix_cx, .OpiEnv$Display$fix_cy,
                circles = .OpiEnv$Display$fix_sx / 2,
                add = TRUE, fg = .OpiEnv$Display$fix_color, 
                lwd = 3, bg = NA, inches = FALSE)
  }
}

#' @rdname opiInitialize
#' @param width    Width of the screen in pixels
#' @param height   Height of the screen in pixels
#' @param ppi      Pixels per inch of the display
#' @param viewdist Viewing distance in cm
#' @details
#' \subsection{Display}{
#'   \code{opiInitialize((width, height, ppi, viewdist, lut = .OpiEnv$Display$LUT)
#'   )}
#'   
#'   If the chosen OPI implementation is \code{Display}, then you can specify
#'   the limits of the plot area and the background color of the plot area.
#'   Note that this assumes \code{link{X11()}} is available on the platform.
#'
#'   We need to know the physical dimensions of the screen and the window
#'   generated in order to calculate stimulus position and size in degrees
#'   of visual angle. The physical dimensions in inches are calculated from
#'   width, height, and ppi. The pixel size pix per degree is then obtained
#'   using viewdist. A gamma function for the screen should be obtained and
#'   its lut passed to convert from luminance in cd/m2 to 8-bit pixel value
#'   (256 levels).
#'
#' }
#' @return
#' \subsection{Display}{
#'   Always returns NULL.
#' }
#' @examples
#' \dontrun{
#'   # Set up a Display and wait for a key press in it.
#'   chooseOpi("Display")
#'   if (!is.null(opiInitialize(width = 1680, height = 1050, ppi = 128, viewdist = 25)))
#'     stop("opiInitialize failed")
#'
#'   opiSetBackground(lum = 100, color = "white", fixation = "Circle")
#'
#'   opiClose()
#' }
display.opiInitialize <- function(width, height, ppi, viewdist, lut = seq(0, 400, length.out = 256)) {
  cmPerInch <- 2.54 # to obtain screen dimensions in cm
  # set defaults
  .OpiEnv$Display$LUT              <- lut # look up table
  .OpiEnv$Display$background_lum   <- 10
  .OpiEnv$Display$background_color <- "black"
  .OpiEnv$Display$fixation         <- "None"
  .OpiEnv$Display$fix_cx           <- 0
  .OpiEnv$Display$fix_cy           <- 0
  .OpiEnv$Display$fix_sx           <- 1
  .OpiEnv$Display$fix_sy           <- 1
  .OpiEnv$Display$fix_color        <- "green"
  # select X11 window manage and open a new device
  .OpiEnv$Display$oldDevice <- options("device") # store device number
  options(device = "X11")
  grDevices::dev.new(title = " ")
  .OpiEnv$Display$devNumber <- grDevices::dev.cur() # store device number
  par(mar = c(0, 0, 0, 0))
  .OpiEnv$Display$width   <- width
  .OpiEnv$Display$height  <- height
  .OpiEnv$Display$ppi     <- ppi
  .OpiEnv$Display$LUT     <- lut
  widthcm   <- cmPerInch * width / ppi
  heightcm  <- cmPerInch * height / ppi
  widthdeg  <- 180 / pi * atan(widthcm / 2 / viewdist)
  heightdeg <- 180 / pi * atan(heightcm / 2 / viewdist)
  # in case we obtain different pixel sizes vertically and horizontally, average then
  pixsize <- (width / widthdeg + height / heightdeg) / 2
  .OpiEnv$Display$pixsize <- pixsize
  # get the maximum visual field that can be tested with the screen
  .OpiEnv$Display$xlimsc  <- width / pixsize * c(-1, 1)
  .OpiEnv$Display$ylimsc  <- height / pixsize * c(-1, 1)
  # allow to resize the screen
  plot(0:0, typ = "n", xaxt = "n", yaxt = "n", ann = FALSE)
  par(usr = c(-1, 1, -1, 1)) # use some arbitrary user coordinate extremes
  opiSetBackground(fixation = "None")
  text(0, 0, "Press a key when resized to your liking", col = "white", cex = 2)
  grDevices::getGraphicsEvent("Waiting for key press", onKeybd = function(key) return(TRUE))
  # check the visual field that can be tested in the current screen. This is key as
  # graph control will depend on these limits
  if(is.null(grDevices::dev.list())) { # if window closed, return NULL and no harm done
    options(device = .OpiEnv$Display$oldDevice)
    return(NULL)
  }
  # else, ensure we are on the right device
  grDevices::dev.set(.OpiEnv$Display$devNumber)
  dims <- grDevices::dev.size("px")
  xlim <- dims[1] / pixsize * c(-1, 1) # x limits in degrees
  ylim <- dims[2] / pixsize * c(-1, 1) # y limits in degrees
  .OpiEnv$Display$xlim <- xlim
  .OpiEnv$Display$ylim <- ylim
  .OpiEnv$Display$fixation <- "Cross"
  par(usr = c(xlim, ylim)) # change the extremes of the user coordinates
  opiSetBackground(fixation = "None")
  print("DO NOT resize the screen again or degrees of visual angle will be all wrong")
  return(NULL)
}

#' @rdname opiPresent
#' @details
#' \subsection{Display}{
#'   Present a circle of radius \code{stim$size} and color \code{stim$color} 
#'   at \code{(stim$x, stim$y)} for \code{stim$duration} ms and wait for a keyboard 
#'   or mouse response for \code{stim$responseWindow} ms.
#'   
#'   \code{stim$size}, \code{sitm$x} and \code{stim$y} are in the same units 
#'   as \code{xlim} and \code{ylim} as specified in \code{\link{opiInitialise}}.
#'   
#'   If the chosen OPI implementation is \code{Display}, then \code{nextStim}
#'   is ignored.
#'   
#'   Duration and response window are rounded to the nearest 5 ms.
#'   
#'   Currently only implemented for \code{opiStaticStimulus}.
#' }
#' @examples
#' \dontrun{
#' # Display a spot
#'  chooseOpi("Display")
#'  if(!is.null(opiInitialize(width = 1680, height = 1050, ppi = 128, viewdist = 25)))
#'    stop("opiInitialize failed")
#'  opiSetBackground(lum = 50, color = "white", fixation = "Cross")
#'
#'  makeStim <- function(db) {
#'    s <- list(x = 9, y = 9, level = dbTocd(db, 400), size = 1.72, color = "white",
#'              duration = 1000, responseWindow = 1000)
#'    class(s) <- "opiStaticStimulus"
#'    return(s)
#'  }
#'  result <- opiPresent(makeStim(0))
#'
#'  opiClose()
#'}
display.opiPresent <- function(stim, nextStim = NULL) { UseMethod("display.opiPresent") }
setGeneric("display.opiPresent")

display.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
  if(is.null(stim)) return(list(err = "No stimulus"))

  if(is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
  if(is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
  if(is.null(stim$size)) return(list(err="No size in stimulus", seen=NA, time=NA))
  if(is.null(stim$level)) return(list(err="No level in stimulus", seen=NA, time=NA))
  if(is.null(stim$color)) return(list(err="No color in stimulus", seen=NA, time=NA))
  if(is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
  if(is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))
  # check if stimulus in bounds
  if(stim$x - stim$size / 2 < .OpiEnv$Display$xlim[1] ||
     stim$x + stim$size / 2 > .OpiEnv$Display$xlim[2] ||
     stim$y - stim$size / 2 < .OpiEnv$Display$ylim[1] ||
     stim$y + stim$size / 2 > .OpiEnv$Display$ylim[2])
    return(list(err="Stimulus out of bounds", seen=NA, time=NA))

  grDevices::dev.set(.OpiEnv$Display$devNumber)

  key <- NA
  pres_done <- FALSE

  hKey <- function(k) key <<- Sys.time()
  hIdle <- function() {
    if(!pres_done && as.numeric(Sys.time() - pres_start_time) >= stim$duration / 1000) {
      pres_done <<- TRUE
      drawBackground()
    }
    if(as.numeric(Sys.time() - pres_start_time) >= stim$responseWindow / 1000) {
      return(FALSE)
    }
  }
        # I don't really understands why this works. hIdle is the key somehow...
  bg <- find_pixel_value_display(stim$level)
  if (stim$color == "red") {
    vals <- bg * c(1, 0, 0)
  } else if (stim$color == "green") {
    vals <- bg * c(0, 1, 0)
  } else if (stim$color == "blue") {
    vals <- bg * c(0, 0, 1)
  } else vals <- bg * c(1, 1, 1) # anything else, means white
  bg <- grDevices::rgb(vals[1], vals[2], vals[3], maxColorValue = 255)
  
  symbols(stim$x, stim$y, circles = stim$size / 2, bg = bg,
          add = TRUE, fg = NA, inches = FALSE)
  pres_start_time <- Sys.time()

  grDevices::getGraphicsEvent("", onKeybd = hKey, onIdle = hIdle)

  if (!is.na(key)) {  # might have to finish the presentation
    while (as.numeric(Sys.time() - pres_start_time) < stim$duration / 1000)
      Sys.sleep(5 / 1000)
    drawBackground()
  }
  return(list(err = NULL, seen = !is.na(key),
              time = ifelse(is.na(key), NA, as.numeric(key - pres_start_time) * 1000)))
}


########################################## 
# Present kinetic stim, return values 
########################################## 
display.opiPresent.opiKineticStimulus <- function(stim, ...) {
    warning("Display does not support kinetic stimuli (yet)")
    return(list(err="Display does not support kinetic stimuli (yet)", seen=FALSE, time=0))
}

###########################################################################
# TODO
###########################################################################
display.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("Display does not support temporal stimuli (yet)")
    return(list(err="Display does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

#' @rdname opiSetBackground
#' @details
#' # Display
#'   \code{opiSetBackground(TODO)}
#' 
#' @return
#' ## Display
#'    Changes the background and the fixation marker.
#' 
#' @examples
#' \dontrun{
#'   # Set up a Display and wait for a key press in it.
#'   chooseOpi("Display")
#'   if (!is.null(opiInitialize(width = 1680, height = 1050, ppi = 128, viewdist = 25)))
#'     stop("opiInitialize failed")
#'
#'   opiSetBackground()
#'   opiSetBackground(lum = 100, color = "white", fixation = "Circle")
#'   opiSetBackground(lum = 100, color = "white", fixation = "Cross", fix_color = "red",
#'                    fix_cx = 2, fix_cy = 5, fix_sx = 1, fix_sy = 2)
#'   opiClose()
#' }
display.opiSetBackground <- function(lum       = .OpiEnv$Display$background_lum,
                                     color     = .OpiEnv$Display$background_color,
                                     fixation  = .OpiEnv$Display$fixation,
                                     fix_cx    = .OpiEnv$Display$fix_cx,
                                     fix_cy    = .OpiEnv$Display$fix_cy,
                                     fix_sx    = .OpiEnv$Display$fix_sx,
                                     fix_sy    = .OpiEnv$Display$fix_sy,
                                     fix_color = .OpiEnv$Display$fix_color) {
  if (!fixation %in% c("Circle", "Cross", "None")) {
      warning("opiSetBackground: invalid fixation, using None")
      assign("fixation", .OpiEnv$Display$FIX_NONE, envir = .OpiEnv$Display)
  }
  .OpiEnv$Display$background_lum   <- lum
  .OpiEnv$Display$background_color <- color
  .OpiEnv$Display$fixation         <- fixation
  .OpiEnv$Display$fix_cx           <- fix_cx
  .OpiEnv$Display$fix_cy           <- fix_cy
  .OpiEnv$Display$fix_sx           <- fix_sx
  .OpiEnv$Display$fix_sy           <- fix_sy
  .OpiEnv$Display$fix_color        <- fix_color
  drawBackground()
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
#' \subsection{Display}{
#'   Shuts the display.
#' }
display.opiClose <- function() {
    grDevices::dev.off(.OpiEnv$Display$devNumber)

    if (!is.na(.OpiEnv$Display$oldDevice))
        options(device=.OpiEnv$Display$oldDevice)

    return(NULL)
}

##############################################################################
#### Lists defined constants
##############################################################################
#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @details
#' \subsection{Display}{
#'   Returns all constants in \code{.OpiEnv$Display} as a list.
#' }
#' \subsection{Display}{
#'   Returns values in use by Display.
#' }
display.opiQueryDevice <- function() {
    vars <- ls(.OpiEnv$Display)
    lst <- lapply(vars, function(i) .OpiEnv$Display[[i]])
    names(lst) <- vars
    return(lst)
}

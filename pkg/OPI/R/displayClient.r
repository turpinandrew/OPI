#
# OPI to display in a plot window.
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: Dec 2019 
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

#' @importFrom grDevices dev.new dev.off dev.set getGraphicsEvent
#' @importFrom graphics lines polygon symbols
NULL


###################################################################
# .OpiEnv$DayDream$socket is the connection to the daydream
# .OpiEnv$DayDream$LUT has 256 entries. LUT[x] is cd/m^2 value for grey level x
# .OpiEnv$DayDream$fovy default is 90
# .OpiEnv$DayDream$...    constants and setting variables
###################################################################
if (exists(".OpiEnv") && !exists("Display", where=.OpiEnv)) {
  assign("Display", new.env(10), envir=.OpiEnv)
  
    .OpiEnv$Display$ylim <- NA
    .OpiEnv$Display$xlim <- NA
    .OpiEnv$Display$backgroundColor <- NA
    .OpiEnv$Display$devNumber <- NA
    .OpiEnv$Display$oldDevice <- NA

    .OpiEnv$Display$FIX_CIRCLE <- 2
    .OpiEnv$Display$FIX_CROSS <- 1
    .OpiEnv$Display$FIX_NONE <- 0

    .OpiEnv$Display$fix_cx  <- 0
    .OpiEnv$Display$fix_cy  <- 0
    .OpiEnv$Display$fix_sx  <- 1
    .OpiEnv$Display$fix_sy  <- 1
    .OpiEnv$Display$fix_color <- c(0,255,0)
    .OpiEnv$Display$fix_type <- .OpiEnv$Display$FIX_NONE
}

drawBackground <- function() {
    p <- par('usr')
    polygon(c(p[1], p[1], p[2], p[2]), c(p[3], p[4], p[4], p[3]), 
        border=NA, col=.OpiEnv$Display$bg)

    if (.OpiEnv$Display$fix_type == .OpiEnv$Display$FIX_CROSS) {
        lines(.OpiEnv$Display$fix_cx + c(-.OpiEnv$Display$fix_sx, .OpiEnv$Display$fix_sx),
            c(.OpiEnv$Display$fix_cy,.OpiEnv$Display$fix_cy),
            col=.OpiEnv$Display$fix_color
        )
        lines(c(.OpiEnv$Display$fix_cx,.OpiEnv$Display$fix_cx),
            .OpiEnv$Display$fix_cy + c(-.OpiEnv$Display$fix_sy, .OpiEnv$Display$fix_sy),
            col=.OpiEnv$Display$fix_color
        )
    } else if (.OpiEnv$Display$fix_type == .OpiEnv$Display$FIX_CIRCLE) {
        symbols(.OpiEnv$Display$fix_cx,
                .OpiEnv$Display$fix_cy,     
                circles=.OpiEnv$Display$fix_sx, 
                add=TRUE, fg=.OpiEnv$Display$fix_color, 
                bg=NA, inches=FALSE)
    } 
}

#' @rdname opiInitialize
#' @param xlim Limits of x coordinates of stimuli and bounds of display area
#' @param ylim Limits of y coordinates of stimuli and bounds of display area
#' @param bg   Background color of display
#' @details
#' \subsection{Display}{
#'   \code{opiInitialize(xlim=c(-30, 30), ylim=c(-25,25), bg=grey(0.5))}
#'   
#'   If the chosen OPI implementation is \code{Display}, then you can specify
#'   the limits of the plot area and the background color of the plot area.
#'   Note that this assumes \code{link{X11()}} is available on the platform.
#' }
#' @return
#' \subsection{Display}{
#'   Always returns NULL.
#' }
#' @examples
#' \dontrun{
#'   # Set up a Display and wait for a key press in it.
#'   chooseOpi("Display")
#'   if (!is.null(opiInitialize()))
#'     stop("opiInitialize failed")
#'
#'   text(0,0, "Press a key when resized to your liking")
#'   getGraphicsEvent("Waiting for key press", onKeybd = function(key) return(TRUE))
#' 
#'   opiSetBackground(color=grey(0.8), fix_type=.OpiEnv$Display$FIX_CIRCLE)
#' }
display.opiInitialize <- function( xlim=c(-30, 30), ylim=c(-25,25), bg=grey(0.5)) {

    assign("xlim", xlim, envir = .OpiEnv$Display)
    assign("ylim", ylim, envir = .OpiEnv$Display)
    assign("bg", bg, envir = .OpiEnv$Display)

    assign("oldDevice", options()$device, envir = .OpiEnv$Display)
    
    options(device="X11")
    dev.new()
    assign("devNumber", dev.cur(), envir = .OpiEnv$Display)
    
    par(mar=c(0,0,0,0))
    plot(0:0, xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="")
    drawBackground()

    return(list(err=NULL))
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
#'     # Display a spot
#'   chooseOpi("Display")
#'   opiInitialise()
#'   text(0,0, "Press a key when ready")
#'   getGraphicsEvent("Waiting for key press", onKeybd = function(key) return(TRUE))
#'
#'   s <- list(x=10, y=10, size=2, color="red", duration=200, responseWindow=1500)
#'   class(s) <- "opiStaticStimulus"
#'   opiPresent(s)
#' }
display.opiPresent <- function(stim, nextStim=NULL) { UseMethod("display.opiPresent") }
setGeneric("display.opiPresent")

display.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) return(list(err = "No stimulus"))

    if (is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$size)) return(list(err="No size in stimulus", seen=NA, time=NA))
    if (is.null(stim$color)) return(list(err="No color in stimulus", seen=NA, time=NA))
    if (is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
    if (is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))

    dev.set(.OpiEnv$Display$devNumber)

    key <- NA
    pres_done <- FALSE

    hKey <- function(k) key <<- Sys.time()
    hMouse <- function(b,x,y) key <<- Sys.time()
    hIdle <- function() {
        if (!pres_done && as.numeric(Sys.time() - pres_start_time) >= stim$duration/1000 ) {
            pres_done <<- TRUE
            drawBackground()
        }
        if (as.numeric(Sys.time() - pres_start_time) >= stim$responseWindow/1000 ) {
            return(FALSE)
        }
    }
        # I don't really understands why this works. hIdle is the key somehow...

    symbols(stim$x, stim$y, circles=stim$size, add=TRUE, fg=NA, bg=stim$color, inches=FALSE)
    pres_start_time <- Sys.time()

    getGraphicsEvent("", onKeybd = hKey, onMouseDown = hMouse, onIdle= hIdle)

    if (!is.na(key)) {  # might have to finish the presentation
        while (as.numeric(Sys.time() - pres_start_time) < stim$duration/1000 )
            Sys.sleep(5/1000)
        drawBackground()
    }

    return(list(
        err  = NULL,
        seen = !is.na(key),    
        time = ifelse(is.na(key), NA, as.numeric(key - pres_start_time) * 1000)
    ))
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
#' @param lum ignored
#' @param color Color of the background. Any valid R plot color.
#' @param fix_type One of \code{.OpiEnv$Display$FIX_NONE}, \code{.OpiEnv$Display$FIX_CROSS}, 
#'                 or \code{.OpiEnv$Display$FIX_CIRCLE}, 
#' @param fix_cx fixation x position in degrees of visual angle. Default is 0
#' @param fix_cy fixation y position in degrees of visual angle. Default is 0
#' @param fix_sx fixation horizontal size in degrees of visual angle. Default is 1. 
#'               If \code{fix_type} is \code{.OpiEnv$Display$FIX_CIRCLE}, then 
#'               this is the radius of the circle.
#' @param fix_sy fixation vertical size in degrees of visual angle. Default is 1
#' @param fix_color fixation color. Any valid R plot color. Default \code{green}.
#' @details
#' \subsection{Display}{
#'   \code{opiSetBackground(lum=NA, color=grey(0.7), fix_type=.OpiEnv$Display$FIX_CROSS, fix_cx=0, fix_cy=0,fix_sx=1, fix_sy=1, fix_color="green")}
#' }
#' @return
#' \subsection{Display}{ 
#'    Changes the background and the fixation marker.
#' }
#' @examples
#' \dontrun{
#'   # Set up a Display and wait for a key press in it.
#'   chooseOpi("Display")
#'   if (!is.null(opiInitialize()))
#'     stop("opiInitialize failed")
#'
#'   opiSetBackground(color=grey(0.8), fix_type=.OpiEnv$Display$FIX_CIRCLE, fix_sx=2)
#' }
display.opiSetBackground <- function(lum=NA, color=grey(0.7), fix_type=.OpiEnv$Display$FIX_CROSS,
                                      fix_cx=0, fix_cy=0, fix_sx=1, fix_sy=1,
                                      fix_color="green") {

    if (! fix_type %in% c(.OpiEnv$Display$FIX_CIRCLE, 
                          .OpiEnv$Display$FIX_CROSS,
                          .OpiEnv$Display$FIX_NONE)) {
        warning("opiSetBackground: invalid fix_type, using None")
        assign("fix_type", .OpiEnv$Display$FIX_NONE, envir=.OpiEnv$Display)
    }
        
    assign("fix_cx"   , fix_cx   , envir= .OpiEnv$Display)
    assign("fix_cy"   , fix_cy   , envir= .OpiEnv$Display)
    assign("fix_sx"   , fix_sx   , envir= .OpiEnv$Display)
    assign("fix_sy"   , fix_sy   , envir= .OpiEnv$Display)
    assign("fix_type" , fix_type , envir= .OpiEnv$Display)
    assign("fix_color", fix_color, envir= .OpiEnv$Display)

    assign("bg", color, envir= .OpiEnv$Display)
    
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
    dev.off(.OpiEnv$Display$devNumber)

    if (!is.na(.OpiEnv$Display$oldDevice))
        options(device=.OpiEnv$Display$oldDevice)

    return(list(err=NULL))
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

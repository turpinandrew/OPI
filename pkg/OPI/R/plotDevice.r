#
# An implementation of the OPI that draws image stimuli to a  
# default graphics device (ie plot window).
# Default device can be got with getOption("device").
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: January 2018
#
# Copyright 2018 Andrew Turpin
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

if (!exists(".OPIPlotEnv"))
    .OPIPlotEnv <- new.env(size=5)


drawWindow <- function() {
    par(bg=.OPIPlotEnv$bg)
    plot(0:1,0:1, type="n", ann=FALSE, axes=FALSE)
}

################################################################################
# Input
#   bg     Background color in any R color format
#   width  in units of device
#   height in units of device
#   responseKeys is a list of valid response key presses.  NA means any key.
#
# Return NULL if succesful, string error message otherwise  
################################################################################
plotDevice.opiInitialize <- function(bg='white', width=6, height=6, responseKeys=NA) {

    if (!has_keypress_support())
        return('No keypress support. If you are using windows, run from cmd line')

    dev.new(width=width, height=height)

    assign("bg", bg, envir = .OPIPlotEnv)
    assign("dev", dev.cur(), envir = .OPIPlotEnv)
    assign("width", width, envir = .OPIPlotEnv)
    assign("height", height, envir = .OPIPlotEnv)
    assign("responseKeys", responseKeys, envir = .OPIPlotEnv)

    if (is.null(dev.cur()))
        return(paste("Could not open default device: ", getOption("device")))

    drawWindow()

    return(NULL)
}

################################################################################
# Set background of plot area to col
# Return:
#   NULL - succsess
#   -1   - opiInitialize not called
################################################################################
plotDevice.opiSetBackground <- function(bg) { 
    assign("bg", bg, envir = .OPIPlotEnv)
    drawWindow()
    return (NULL)
}

################################################################################
# generic opiPresent
################################################################################
plotDevice.opiPresent <- function(stim, nextStim=NULL) { UseMethod("plotDevice.opiPresent") }
setGeneric("plotDevice.opiPresent")

    # wait stopAt ms for some keypress in responseKeys
waiter <- function(stopAt) {
    st <- Sys.time()
    k <- keypress(block=FALSE)
    while (!k %in% .OPIPlotEnv$responseKeys && Sys.time() - st < stopAt) {
        k <- keypress(block=FALSE)
        #print(paste("Wait", k, Sys.time() - st))
        Sys.sleep(1/1000)
    }
    print(k)

    return(list(time=Sys.time() - st, key=k))
}

################################################################################
# opiPresent for static stimuli
################################################################################
plotDevice.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL) {
    if (! 'image'    %in% names(stim)) return(list(err="No image in stim for opiPresent."))
    if (! 'duration' %in% names(stim)) return(list(err="No duration in stim for opiPresent."))
    if (! 'responseWindow' %in% names(stim)) return(list(err="No responseWindow in stim for opiPresent."))

    rasterImage(stim$image,0,0,1,1)

    res <- waiter(stim$duration/1000)

    drawWindow()

    if (res$key == "none") {
        res <- waiter(stim$responseWindow/1000)
    } else {
        return (list(err = NULL, seen= TRUE, time= as.double(res$time)))
    }

    if (res$key == "none")
        return (list(err = NULL, seen= FALSE, time= -1))
    else {
        return (list(err = NULL, seen= TRUE, time=as.double(res$time)))
    }
}

################################################################################
# opiPresent for temporal stimuli
################################################################################
plotDevice.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL) {
    if (! 'image'    %in% names(stim)) return(list(err="No image in stim for opiPresent."))
    if (! 'rate'     %in% names(stim)) return(list(err="No rate in stim for opiPresent."))
    if (! 'lut'      %in% names(stim)) return(list(err="No lut in stim for opiPresent."))
    if (! 'duration' %in% names(stim)) return(list(err="No duration in stim for opiPresent."))
    if (! 'responseWindow' %in% names(stim)) return(list(err="No responseWindow in stim for opiPresent."))

    rasterImage(stim$image,0,0,1,1)

    res <- waiter(stim$duration/1000)

    drawWindow()

    if (res$key == "none") {
        res <- waiter(stim$responseWindow/1000)
    } else {
        return (list(err = NULL, seen= TRUE, time= as.double(res$time)))
    }

    if (res$key == "none")
        return (list(err = NULL, seen= FALSE, time= -1))
    else {
        return (list(err = NULL, seen= TRUE, time=as.double(res$time)))
    }
}

################################################################################
#
################################################################################
plotDevice.opiClose         <- function() { dev.off(.OPIPlotEnv$dev) ; return(NULL) }

################################################################################
#
################################################################################
plotDevice.opiQueryDevice   <- function() { return (list(dev=.OPIPlotEnv$dev)) }

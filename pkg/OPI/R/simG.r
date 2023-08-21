#
# An implementation of the OPI that simulates responses using
# Cumulative Gaussian variability with supplied standard deviation.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: June 2012
#
# Modified 20 Jul 2014: added maxStim argument for cdTodB conversion
#
# Copyright [2022] [Andrew Turpin]
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


#' @rdname opiClose
#' @return
#' \subsection{SimGaussian}{
#'   DETAILS
#' }
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#' if (!is.null(opiClose()))
#'   stop("opiClose failed, which is very surprising!")
simG.opiClose         <- function() { return(NULL) }

#' @rdname opiQueryDevice
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#' print(opiQueryDevice())
simG.opiQueryDevice   <- function() { return (list(type="SimGaussian", isSim=TRUE)) }

if (exists(".OpiEnv") && !exists("SimG", where=.OpiEnv))
    assign("SimG", new.env(2), envir=.OpiEnv)

################################################################################
# Input
#   sd standard deviation for the Gaussian
#   display Dimensions of plot area to display stim. No display if NULL
#
# Return NULL if succesful, string error message otherwise  
################################################################################
#' @rdname opiInitialize
#' @param sd standard deviation for the Gaussian
#' @details
#' \subsection{SimGaussian}{
#'   \code{opiInitialize(sd, display=NA, maxStim=10000/pi)}
#'   
#'   If the chosen OPI implementation is \code{SimGaussian}, then \code{sd} is the
#'   standard deviation value that the simulator will use for the slope/spread of
#'   the psychometric function.
#'   
#'   \code{display} and \code{maxStim} is as for SimHenson.
#' }
#' @examples
#' # Set up a simulation using a psychometric function that is
#' # a cumulative gaussian of standard deviation 2
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
simG.opiInitialize <- function(sd = 2, display = NA, maxStim = 10000 / pi) {
    if (!is.numeric(sd) || (sd < 0)) {
        msg <- paste("Invalid standard deviation in opiInitialize for SimGaussian:",sd)
        warning(msg)
        return(msg)
    }

    .OpiEnv$SimG$sd <- sd
    .OpiEnv$SimG$maxStim <- maxStim

    if (simDisplay.setupDisplay(display))
        warning("opiInitialize (SimGaussian): perhaps display parameter does not contain 4 numbers?")

    return(NULL)
}

################################################################################
# Set background of plot area to col, color of gird lines to gridCol
################################################################################
#' @rdname opiSetBackground
#' @details
#' \subsection{SimGaussian}{
#'   \code{opiSetBackground(col, gridCol)}
#'   
#'   \code{col} is the background color of the plot area used for displaying
#'   stimuli, and \code{gridCol} the color of the gridlines. Note the plot area
#'   will only be displayed if \code{opiInitialize} is called with a valid display
#'   argument.
#' }
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#' if (!is.null(opiSetBackground(col="white",gridCol="grey")))
#'   stop("opiSetBackground failed, which is very surprising!")
simG.opiSetBackground <- function(col, gridCol) { 
    simDisplay.setBackground(col, gridCol)
    return(NULL) 
}

#' @rdname opiPresent
#' @details
#' \subsection{SimGaussian}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}
#'   
#'   If the chosen OPI implementation is \code{SimGaussian}, then the response
#'   to a stimuli is determined by sampling from a Frequency-of-Seeing (FoS)
#'   curve (also known as the psychometric function) with formula
#'   \code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, simG.global.sd))}, where \code{x}
#'   is the stimulus value in Humphrey dB, and \code{simG.global.sd} is
#'   set with \code{opiInitialize}.
#' }
simG.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) { UseMethod("simG.opiPresent") }
setGeneric("simG.opiPresent")

simG.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) {
    if (!exists("sd", envir=.OpiEnv$SimG)) {
        return ( list(
            err = "opiInitialize(sd) was not called before opiPresent()",
            seen= NA,
            time= NA 
        ))
    }

    if (is.null(stim))
        stop("stim is NULL in call to opiPresent (using simGaussian, opiStaticStimulus)")

    if (as.numeric(.OpiEnv$SimG$sd) <= 0)
        warning("sd is <= 0 in SimGaussian call to opiPresent")

    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(cdTodb(stim$level, .OpiEnv$SimG$maxStim), mean=tt, sd=as.numeric(.OpiEnv$SimG$sd)))

    simDisplay.present(stim$x, stim$y, stim$color, stim$duration, stim$responseWindow)

    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}#

########################################## TO DO
simG.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simG temporal persenter yet")
}

simG.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simG kinetic persenter yet")
}

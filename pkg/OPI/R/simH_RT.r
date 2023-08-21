#
# An implementation of the OPI that simulates responses using
# Henson et al (2000) variability and also returns response times
# using data from McKendrick et al 2014.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: August 2013
#
# Modified Tue  8 Jul 2014: added type="X" to opiInitialise and opiPresent
# Modified 20 Jul 2014: added maxStim argument for cdTodB conversion
# Modified September 2016: Added kinetic
# Modified October 2016: Completely changed kinetic
# Modified February 2017: Moved kinetic out to simH.r
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

simH_RT.opiClose         <- function() { return(NULL) }
simH_RT.opiQueryDevice   <- function() { return (list(type="SimHensonRT", isSim=TRUE)) }

if (exists(".OpiEnv") && !exists("SimHRT", where=.OpiEnv))
    assign("SimHRT", new.env(5), envir=.OpiEnv)

################################################################################
# Input
#   type N|G|C for the three Henson params
#   type X to specify your own A and B values (eg different dB scale)
#   cap  dB value for capping stdev form Henson formula
#   display Dimensions of plot area (-x,+x,-y,+y) to display stim. No display if NULL
#   rtData data.frame with colnames == "Rt", "Dist", "Person"
#
# Side effects if successful:
#   Set .OpiEnv$SimHRT$type   to type
#   Set .OpiEnv$SimHRT$cap    to cap
#   Set .OpiEnv$SimHRT$A      to A
#   Set .OpiEnv$SimHRT$B      to B
#   Set .OpiEnv$SimHRT$rtData to 3 col data frame to rtData
#
# Return NULL if successful, string error message otherwise  
################################################################################
#' @rdname opiInitialize
#' @param rtData data.frame with colnames == "Rt", "Dist", "Person" for "SimHensonRT"
#' @param rtFP response time for false positives ??? for "SimHensonRT"
#' @details
#' \subsection{SimHensonRT}{
#'   \code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NA, maxStim=10000/pi, rtData, rtFP=1:1600)}
#'   
#'   If the chosen OPI implementation is \code{SimHensonRT}, then the first six
#'   parameters are as in \code{SimHenson}, and \code{rtData} is a data frame
#'   with at least 2 columns: \code{"Rt"}, reponse time; and \code{"Dist"},
#'   signifying that distance between assumed threshold and stimulus value in
#'   your units.
#'   
#'   This package contains \code{RtSigmaUnits} or \code{RtDbUnits} that can be
#'   loaded with the commands \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)},
#'   and are suitable to pass as values for \code{rtData}.
#'   
#'   \code{rtFp} gives the vector of values in milliseconds from which a response
#'   time for a false positive response is randomly sampled.
#' }
#' @examples
#' # Set up a simple simulation for white-on-white perimetry
#' # and display the stimuli in a plot region and simulate response times
#' chooseOpi("SimHensonRT")
#' data(RtSigmaUnits)
#' oi <- opiInitialize(type="C", cap=6, display=NA, rtData=RtSigmaUnits, rtFP=1:100)
#' if (!is.null(oi))
#'   stop("opiInitialize failed")
simH_RT.opiInitialize <- function(type = "C", cap = 6, A = -0.081, B = 3.27,
                                  display = NA,
                                  maxStim = 10000 / pi,
                                  rtData = NULL, rtFP = 1:1600) {
    if (!is.element(type,c("N","G","C", "X"))) {
        msg <- paste("Bad 'type' specified for SimHensonRT in opiInitialize():",type)
        warning(msg)
        return(msg)
    }
    
    if (type == "N") {  A <- -0.066 ; B <- 2.81 } 
    else if (type == "G") { A <- -0.098 ;  B <- 3.62    } 
    else if (type == "C") { A <- -0.081 ;  B <- 3.27    }

    if (cap < 0)
        warning("cap is negative in call to opiInitialize (SimHensonRT)")
    
    .OpiEnv$SimHRT$type <- type
    .OpiEnv$SimHRT$cap  <-  cap
    .OpiEnv$SimHRT$A    <-  A
    .OpiEnv$SimHRT$B    <-  B
    .OpiEnv$SimHRT$maxStim <- maxStim

    if (type == "X" && (is.na(A) || is.na(B)))
        warning("opiInitialize (SimHenson): you have chosen type X, but one/both A and B are NA")
      
    if(simDisplay.setupDisplay(display))
        warning("opiInitialize (SimHensonRT): display parameter may not contain 4 numbers.")

    #if (rtType == "sigma") {
    #    load(paste(.Library,"/OPI/data/RtSigmaUnits.RData",sep=""), envir=.OpiEnv$SimHRT)
    #    assign("rtData", .OpiEnv$SimHRT$RtSigmaUnits, envir=.OpiEnv$SimHRT)
    #} else if (rtType == "db") {
    #    load(paste(.Library,"/OPI/data/RtDbUnits.RData",sep=""), envir=.OpiEnv$SimHRT)
    #    assign("rtData", .OpiEnv$SimHRT$RtDbUnits, envir=.OpiEnv$SimHRT)
    #} else {
    #    msg <- paste("opiInitialize (SimHensonRT): unknown response time data type",rtType)
    #    warning(msg)
    #    return(msg)
    #}

    if (nrow(rtData) < 100) 
        warning("opiInitialize (SimHensonRT): Less than 100 rows in rtData; that's wierd")
    if (ncol(rtData) != 3 || !all(colnames(rtData) == c("Rt", "Dist", "Person"))) {
        msg <- "opiInitialize (SimHensonRT): rtData must have 3 columns: Rt, Dist, Person. See data(RtSigmaUnits) for example."
        warning(msg)
        return(msg)
    }
    assign("rtData", rtData, envir=.OpiEnv$SimHRT)

    #print(.OpiEnv$SimHRT$rtData[1:10,])

    if (length(rtFP) < 1) {
        msg <- "opiInitialize (SimHensonRT): rtFP must have at least 1 element"
        warning(msg)
        return(msg)
    }

    assign("rtFP", rtFP, envir=.OpiEnv$SimHRT)

    return(NULL)
}

################################################################################
# Set background of plot area to col
# Return:
#   NULL - succsess
#   -1   - opiInitialize not called
################################################################################
#' @rdname opiSetBackground
#' @details
#' \subsection{SimHensonRT}{
#'   \code{opiSetBackground(col, gridCol)}
#'   
#'   \code{col} is the background color of the plot area used for displaying
#'   stimuli, and \code{gridCol} the color of the gridlines. Note the plot area
#'   will only be displayed if \code{opiInitialize} is called with a valid display
#'   argument.
#' }
simH_RT.opiSetBackground <- function(col, gridCol) { 
    return (simDisplay.setBackground(col, gridCol))
}

#' @rdname opiPresent
#' @param notSeenToSeen SOMETHING for OPI implementation "SimHensonRT"
#' @details
#' \subsection{SimHensonRT}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt)}
#' 
#'   For static stimuli, this function is the same as for \code{SimHenson}, but
#'   reaction times are determined by sampling from \code{rtData} as passed to
#'   \code{opiInitialize}.  The \code{dist} parameter is the distance of the
#'   stimulus level from the true threshold, and should be in the same units as
#'   the \code{Dist} column of \code{rtData}. The default is just the straight
#'   difference between the stimulus level and the true threshold, but you might
#'   want it scaled somehow to match \code{rtData}.
#' }
#' @examples
#' # Same but with simulated reaction times
#' chooseOpi("SimHensonRT")
#' data(RtSigmaUnits)
#' if (!is.null(opiInitialize(type="C", cap=6, rtData=RtSigmaUnits)))
#'   stop("opiInitialize failed")
#'
#' dist <- (10 - 30)/min(exp(-0.098 * 30 + 3.62), 6)
#' result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01, dist=dist)
#' 
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
simH_RT.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, notSeenToSeen=TRUE) { 
                            UseMethod("simH_RT.opiPresent") }
setGeneric("simH_RT.opiPresent")

#
# Helper function that allows different coefficients from Table 1 of Henson 2000.
# Note prob seeing <0 is 1 (but false neg still poss)
# Response time for false positive is uniform sample from .OpiEnv$SimHRT$rtFP
#
# @param tt - true threshold (in dB)
#                If <0 always seen (unless fn) 
#                If NA always not seen (unless fp)
# @param dist - distance from threshold in appropriate units
#
simH_RT.present <- function(db, fpr=0.03, fnr=0.01, tt=30, dist) {

    falsePosRt <- function() {
        if(length(.OpiEnv$SimHRT$rtFP) < 2) 
            return(.OpiEnv$SimHRT$rtFP)
        else
            return(sample(.OpiEnv$SimHRT$rtFP,size=1))
    }

    if (!is.na(tt) && tt < 0)         # force false pos if t < 0
        fpr <- 1.00  

    fn_ret <- fp_ret <- NULL

    if (runif(1) < fpr) 
        fp_ret <- list(err=NULL, seen=TRUE, time=falsePosRt())  # false P

    if (runif(1) < fnr)
        fn_ret <- list(err=NULL, seen=FALSE, time=0)            # false N

    if (runif(1) < 0.5) {
        if (!is.null(fp_ret)) return(fp_ret)   # fp first, then fn
        if (!is.null(fn_ret)) return(fn_ret)
    } else {
        if (!is.null(fn_ret)) return(fn_ret)   # fn first, then fp
        if (!is.null(fp_ret)) return(fp_ret)
    }

    if (is.na(tt))
        return(list(err=NULL, seen=FALSE, time=0))

        # if get to here then need to check Gaussian
        # and if seen=TRUE need to get a time from .OpiEnv$SimHRT$rtData
        # assume pxVar is sigma for RT is in sigma units
    pxVar <- min(.OpiEnv$SimHRT$cap, exp(.OpiEnv$SimHRT$A*tt + .OpiEnv$SimHRT$B)) # variability of patient, henson formula 
#print(paste(db,tt,pxVar, .OpiEnv$SimHRT$cap, .OpiEnv$SimHRT$A*tt ,.OpiEnv$SimHRT$B))
    if ( runif(1) < 1 - pnorm(db, mean=tt, sd=pxVar)) {

        o <- head(order(abs(.OpiEnv$SimHRT$rtData$Dist - dist)), 100)

        return(list(err=NULL, seen=TRUE, time=sample(.OpiEnv$SimHRT$rtData[o,"Rt"], size=1)))
    } else {
        return(list(err=NULL, seen=FALSE, time=0))
    }
}#simH_RT.present()

#
# stim is list of type opiStaticStimulus
#
simH_RT.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt) {
    if (!exists("type", envir=.OpiEnv$SimHRT)) {
        return ( list(
            err = "opiInitialize(type,cap) was not called before opiPresent()",
            seen= NA,
            time= NA 
        ))
    }

    if (is.null(stim))
        stop("stim is NULL in call to opiPresent (using SimHensonRT, opiStaticStimulus)")

    if (length(tt) != length(fpr))
        warning("In opiPresent (using SimHensonRT), recycling tt or fpr as lengths differ")
    if (length(tt) != length(fnr))
        warning("In opiPresent (using SimHensonRT), recycling tt or fnr as lengths differ")
    if (length(fpr) != length(fnr))
        warning("In opiPresent (using SimHensonRT), recycling fpr or fnr as lengths differ")

    simDisplay.present(stim$x, stim$y, stim$color, stim$duration, stim$responseWindow)

    return(simH_RT.present(cdTodb(stim$level, .OpiEnv$SimHRT$maxStim), fpr, fnr, tt, dist))
}

########################################## TO DO !
simH_RT.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH_RT temporal persenter yet")
}

simH_RT.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH_RT kinetic persenter yet")
}

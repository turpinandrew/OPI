#
# An implementation of the OPI that simulates responses using 
# Henson et al (2000) variability and also returns response times
# using data from McKednrick et al 2014.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: August 2013
#
# Modified Tue  8 Jul 2014: added type="X" to opiInitialise and opiPresent
# Modified 20 Jul 2014: added maxStim argument for cdTodB conversion
# Modified September 2016: Added kinetic
# Modified October 2016: Completely changed kinetic
#
# Copyright 2012 Andrew Turpin
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
#

simH_RT.opiClose         <- function() { return(NULL) }
simH_RT.opiQueryDevice   <- function() { return (list(type="SimHensonRT")) }

if (!exists(".SimHRTEnv"))
    .SimHRTEnv <- new.env(size=5)

################################################################################
# Input
#   type N|G|C for the three Henson params
#   type X to specify your own A and B values (eg different dB scale)
#   cap  dB value for capping stdev form Henson formula
#   display Dimensions of plot area (-x,+x,-y,+y) to display stim. No display if NULL
#   rtData data.frame with colnames == "Rt", "Dist", "Person"
#
# Side effects if successful:
#   Set .SimHRTEnv$type   to type
#   Set .SimHRTEnv$cap    to cap
#   Set .SimHRTEnv$A      to A
#   Set .SimHRTEnv$B      to B
#   Set .SimHRTEnv$rtData to 3 col data frame to rtData
#
# Return NULL if successful, string error message otherwise  
################################################################################
simH_RT.opiInitialize <- function(type="C", cap=6, A=NA, B=NA, display=NULL, maxStim=10000/pi, rtData, rtFP=1:1600) {
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
    
    .SimHRTEnv$type <- type
    .SimHRTEnv$cap  <-  cap
    .SimHRTEnv$A    <-  A
    .SimHRTEnv$B    <-  B
    .SimHRTEnv$maxStim <- maxStim

    if (type == "X" && (is.na(A) || is.na(B)))
        warning("opiInitialize (SimHenson): you have chosen type X, but one/both A and B are NA")
      
    if(simDisplay.setupDisplay(display))
        warning("opiInitialize (SimHensonRT): display parameter may not contain 4 numbers.")

    #if (rtType == "sigma") {
    #    load(paste(.Library,"/OPI/data/RtSigmaUnits.RData",sep=""), envir=.SimHRTEnv)
    #    assign("rtData", .SimHRTEnv$RtSigmaUnits, envir=.SimHRTEnv)
    #} else if (rtType == "db") {
    #    load(paste(.Library,"/OPI/data/RtDbUnits.RData",sep=""), envir=.SimHRTEnv)
    #    assign("rtData", .SimHRTEnv$RtDbUnits, envir=.SimHRTEnv)
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
    assign("rtData", rtData, envir=.SimHRTEnv)

    #print(.SimHRTEnv$rtData[1:10,])

    if (length(rtFP) < 1) {
        msg <- "opiInitialize (SimHensonRT): rtFP must have at least 1 element"
        warning(msg)
        return(msg)
    }

    assign("rtFP", rtFP, envir=.SimHRTEnv)

    return(NULL)
}

################################################################################
# Set background of plot area to col
# Return:
#   NULL - succsess
#   -1   - opiInitialize not called
################################################################################
simH_RT.opiSetBackground <- function(col, gridCol) { 
    return (simDisplay.setBackground(col, gridCol))
}

################################################################################
#
################################################################################
simH_RT.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, notSeenToSeen=TRUE) { 
                            UseMethod("simH_RT.opiPresent") }
setGeneric("simH_RT.opiPresent")

#
# Helper function that allows different coefficients from Table 1 of Henson 2000.
# Note prob seeing <0 is 1 (but false neg still poss)
# Response time for false positive is uniform sample from .SimHRTEnv$rtFP
#
# @param tt - true threshold (in dB)
#                If <0 always seen (unless fn) 
#                If NA always not seen (unless fp)
# @param dist - distance from threshold in appropriate units
#
simH_RT.present <- function(db, fpr=0.03, fnr=0.01, tt=30, dist) {

    falsePosRt <- function() {
        if(length(.SimHRTEnv$rtFP) < 2) 
            return(.SimHRTEnv$rtFP)
        else
            return(sample(.SimHRTEnv$rtFP,size=1))
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
        # and if seen=TRUE need to get a time from .SimHRTEnv$rtData
        # assume pxVar is sigma for RT is in sigma units
    pxVar <- min(.SimHRTEnv$cap, exp(.SimHRTEnv$A*tt + .SimHRTEnv$B)) # variability of patient, henson formula 
#print(paste(db,tt,pxVar, .SimHRTEnv$cap, .SimHRTEnv$A*tt ,.SimHRTEnv$B))
    if ( runif(1) < 1 - pnorm(db, mean=tt, sd=pxVar)) {

        o <- head(order(abs(.SimHRTEnv$rtData$Dist - dist)), 100)

        return(list(err=NULL, seen=TRUE, time=sample(.SimHRTEnv$rtData[o,"Rt"], size=1)))
    } else {
        return(list(err=NULL, seen=FALSE, time=0))
    }
}#simH_RT.present()

#
# stim is list of type opiStaticStimulus
#
simH_RT.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt) {
    if (!exists("type", envir=.SimHRTEnv)) {
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

    return(simH_RT.present(cdTodb(stim$level, .SimHRTEnv$maxStim), fpr, fnr, tt, dist))
}

########################################## TO DO !
simH_RT.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH_RT temporal persenter yet")
}

##################################################################
# Assumes static thresholds/FoS curves and static reaction times.
# Note that false positives and false negatives 
# have to be treated separately from the static responses.
# The location of a false positive is randomly drawn from any 
# location where tt is NA, or prob seeing is < FP_TOLERANCE.
#
# @param tt - a list of vectors that give true threshold as a 
#             evenly spaced along the path (including start and end,
#             tt=NA is never seen)
# @param fpr - false positive rate in [0,1] (note for whole path)
# @param fnr - false negative rate in [0,1] (note for whole path)
# @param tt - list of vectors of true thresholds for each path
# @param dist - function for computing distance from threshold
# @param notSeenToSeen - if TRUE, use prob seeing at each sample, else 1-prob seeing
# @param SAMPLING_SPEED - temporary
##################################################################
simH_RT.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=NULL, dist=function(l,t) l-dbTocd(t), notSeenToSeen=TRUE, SAMPLING_SPEED=50, SEEING_CRITERIA=0, IN_A_ROW=1) {
    if (is.null(stim))
        stop("stim is NULL in call to opiPresent (using SimHensonRT, opiKineticStimulus)")
    if (!is.null(nextStim))
        stop("nextStim should be NULL for kinetic in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

    if (is.null(tt))
        stop("tt must be a list of functions in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

    if (is(tt)[1] != "list")
        stop("tt must be a *list* of vectors in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

    num_paths <- length(stim$path$x) - 1

    if (length(stim$path$y) != num_paths + 1)
        stop(paste("y is length ",length(stim$path$y), "and should be", num_paths+1, "in SimHensonRT - kinetic"))
    if (length(stim$sizes) != num_paths)
        stop(paste("sizes is length ",length(stim$sizes), "and should be", num_paths, "in SimHensonRT - kinetic"))
    if (length(stim$colors) != num_paths)
        stop(paste("colors is length ",length(stim$colors), "and should be", num_paths, "in SimHensonRT - kinetic"))
    if (length(stim$levels) != num_paths)
        stop(paste("levels is length ",length(stim$levels), "and should be", num_paths, "in SimHensonRT - kinetic"))
    if (length(stim$speeds) != num_paths)
        stop(paste("speeds is length ",length(stim$speeds), "and should be", num_paths, "in SimHensonRT - kinetic"))

    ############################################################################## 
    # Build list of (x,y,time, speed, pr_seeing) for granularity steps along each segment
    # assuming a check every SAMPLING_SPEED milliseconds
    ############################################################################## 
    eDistP <- function(x1,y1,x2,y2) sqrt((x1-x2)^2 + (y1-y2)^2) 
    eDistI <- function(i,j) eDistP(xs[j], ys[j], xs[i], ys[i]) 

    #SAMPLING_SPEED <- 50

    xytps <- NULL
    time <- 0
    for (path_num in 1:num_paths) {
        path_len <- eDistP(stim$path$x[path_num], stim$path$y[path_num],
                           stim$path$x[path_num+1], stim$path$y[path_num+1])

        time_for_path <- path_len / stim$speeds[path_num] * 1000

        granularity <- floor(time_for_path / SAMPLING_SPEED)

        xs <- seq(stim$path$x[path_num], stim$path$x[path_num+1], length.out=granularity)
        ys <- seq(stim$path$y[path_num], stim$path$y[path_num+1], length.out=granularity)

        if (eDistI(1, length(xs)) == 0) {
            warning("Path of zero length in kinetic stim. Returning not-seen in SimHensonRT - kinetic") 
            return(list(err=NULL, seen=FALSE, time=NA, x=NA, y=NA))
        }
        
        tt.dists <- seq(0, 1, length.out=length(tt[[path_num]])) * eDistI(1, length(xs))
        tt_noNAs <- tt[[path_num]]   # turn NAs into -1
        z <- is.na(tt_noNAs)
        tt_noNAs[z] <- -1

        db <- cdTodb(stim$levels[path_num], .SimHRTEnv$maxStim)

        time_between_checks <- eDistI(1, 2) / stim$speeds[path_num] * 1000

        for (i in 1:granularity) {
            tt.single <- approx(tt.dists, tt_noNAs, eDistI(1,i))$y

            pr_press <- 0
            if (tt.single >= 0) {
                    # variability of patient, Henson formula 
                pxVar <- min(.SimHRTEnv$cap, exp(.SimHRTEnv$A*tt.single + .SimHRTEnv$B)) 
                pr_press <- 1 - pnorm(db, mean=tt.single, sd=pxVar)
            }

            if (!notSeenToSeen)
                pr_press <- 1 - pr_press

            xytps <- c(xytps, list(list(x=xs[i], y=ys[i], s=stim$speeds[[path_num]], t=time, 
                                        pr= pr_press, 
                                        d=dist(stim$levels[[path_num]], tt.single),
                                        tt=tt.single
            )))

            time <- time + time_between_checks
        }
    }

    #######################################################
    # Check for false repsonses. Randomise order to check.
    ######################################################
    fn_ret <- list(err=NULL, seen=FALSE, time=NA, x=NA, y=NA)

    pr_not_see <- 1 - unlist(lapply(xytps, "[", "pr"))
    if (length(pr_not_see) == 1) {
        loc <- 1
    } else {
        if (all(pr_not_see == 0))
            loc <- sample(1:length(pr_not_see), 1)
        else
            loc <- sample(1:length(pr_not_see), 1, prob=pr_not_see)
    }

    fp_ret <- list(err=NULL,
               seen=TRUE,
               time=xytps[[loc]]$t,
               x=xytps[[loc]]$x,
               y=xytps[[loc]]$y
           )

    if (runif(1) < 0.5) {
        if (runif(1) < fpr) return(fp_ret)   # fp first, then fn
        if (runif(1) < fnr) return(fn_ret)
    } else {
        if (runif(1) < fnr) return(fn_ret)   # fn first, then fp
        if (runif(1) < fpr) return(fp_ret)
    }

    ######################################################
    # Now just walk along xytps flipping coin until seen or get to end.
    # If seen, add a little lag for reaction.
    ######################################################
#print(unlist(lapply(xytps, "[", "pr")))
#print(length(xytps))

    rs <- runif(length(xytps))
    i <- head(which(rs < unlist(lapply(xytps, "[", "pr")) & rs >= SEEING_CRITERIA ), 1)

    in_a_row <- 0
    for (seen in i) {
        if (seen)
            in_a_row <- in_a_row + 1
        else
            in_a_row <- 0
        if (in_a_row == IN_A_ROW)
            break
    }

    if (in_a_row != IN_A_ROW)   # not seen
        return(list(err=NULL, seen=FALSE, time=NA, x=NA, y=NA))

    if (i > 1)
        path_angle <- atan2(xytps[[i]]$y-xytps[[i-1]]$y, xytps[[i]]$x-xytps[[i-1]]$x)
    else
        path_angle <- atan2(xytps[[2]]$y-xytps[[1]]$y, xytps[[2]]$x-xytps[[1]]$x)

    times <- head(order(abs(.SimHRTEnv$rtData$Dist - xytps[[i]]$d)), 100)
    time <- sample(.SimHRTEnv$rtData[times, "Rt"], size=1)

    dist_traveled <- time /1000 * xytps[[i]]$s

    return(list(err=NULL,
                seen=TRUE,
                time=time + xytps[[i]]$t, 
                x=xs[i] + dist_traveled * cos(path_angle),
                y=ys[i] + dist_traveled * sin(path_angle)
    ))
}

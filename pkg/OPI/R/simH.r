#
# An implementation of the OPI that simulates responses using 
# Henson et al (2000) variability.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
#
# Modified  8 Jul 2014: added type="X" to opiInitialise and opiPresent
# Modified 20 Jul 2014: added maxStim argument for cdTodB conversion
# Modified Feburary 2017: Moved kinetic over from simH_RT.r
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

simH.opiClose         <- function() { return(NULL) }
simH.opiQueryDevice   <- function() { return (list(type="SimHenson", isSim=TRUE)) }

if (exists(".OpiEnv") && !exists("SimH", where=.OpiEnv))
    assign("SimH", new.env(), envir=.OpiEnv)

################################################################################
# Input
#   type N|G|C for the three Henson params
#   type X to specify your own A and B values (eg different dB scale)
#   cap  dB value for capping stdev form Henson formula
#   display Dimensions of plot area (-x,+x,-y,+y) to display stim. No display if NULL
#   maxStim Maximum stimulus value in cd/m^2 used for db <-> cd/m^2 conversions
#
# Return NULL if succesful, string error message otherwise  
################################################################################
simH.opiInitialize <- function(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi) {
    if (!is.element(type,c("N","G","C","X"))) {
        msg <- paste("Bad 'type' specified for SimHenson in opiInitialize():",type)
        warning(msg)
        return(msg)
    }

    if (cap < 0)
        warning("cap is negative in call to opiInitialize (simHenson)")

    assign("type",    type, envir = .OpiEnv$SimH)
    assign("cap",     cap, envir = .OpiEnv$SimH)
    assign("maxStim", maxStim, envir = .OpiEnv$SimH)

    if (.OpiEnv$SimH$type == "N") {
        assign("A", -0.066, envir=.OpiEnv$SimH)
        assign("B", 2.81, envir=.OpiEnv$SimH)
    } else if (.OpiEnv$SimH$type == "G") {
        assign("A", -0.098 , envir=.OpiEnv$SimH)
        assign("B", 3.62, envir=.OpiEnv$SimH)
    } else if (.OpiEnv$SimH$type == "C") {
        assign("A", -0.081, envir=.OpiEnv$SimH)
        assign("B", 3.27, envir=.OpiEnv$SimH)
    } else if (.OpiEnv$SimH$type == "X") {
        assign("A" ,  A, envir=.OpiEnv$SimH)
        assign("B",  B, envir=.OpiEnv$SimH)
    }

    if (type == "X" && (is.na(A) || is.na(B)))
        warning("opiInitialize (SimHenson): you have chosen type X, but one/both A and B are NA")

    if(simDisplay.setupDisplay(display))
        warning("opiInitialize (SimHenson): display parameter may not contain 4 numbers.")

    return(NULL)
}

################################################################################
# Set background of plot area to col
# Return:
#   NULL - succsess
#   -1   - opiInitialize not called
################################################################################
simH.opiSetBackground <- function(col, gridCol) { 
    return (simDisplay.setBackground(col, gridCol))
}

################################################################################
#
################################################################################
simH.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30,
                            criteria=0.97, rt_shape=5.3, rt_rate=1.4, rt_scale=0.1) {
                            UseMethod("simH.opiPresent") }
setGeneric("simH.opiPresent")

#
# Helper function that allows different coefficients from Table 1 of Henson 2000.
# Note prob seeing <0 is always false positive rate 
#
simH.present <- function(db, cap=6, fpr=0.03, fnr=0.01, tt=30, A, B) {

    if (tt >= 0) {
        pxVar <- min(cap, exp(A*tt + B)) # variability of patient, henson formula 

        prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(db, mean=tt, sd=pxVar))    
    } else {
        prSeeing <- fpr
    }
    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}#

#
# stim is list of type opiStaticStimulus
#
simH.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) {
    if (!exists("type", envir=.OpiEnv$SimH)) {
        return ( list(
            err = "opiInitialize(type,cap) was not called before opiPresent()",
            seen= NA,
            time= NA 
        ))
    }

    if (is.null(stim))
        stop("stim is NULL in call to opiPresent (using simHenson, opiStaticStimulus)")

    if (length(tt) != length(fpr))
        warning("In opiPresent (using simHenson), recycling tt or fpr as lengths differ")
    if (length(tt) != length(fnr))
        warning("In opiPresent (using simHenson), recycling tt or fnr as lengths differ")
    if (length(fpr) != length(fnr))
        warning("In opiPresent (using simHenson), recycling fpr or fnr as lengths differ")

    simDisplay.present(stim$x, stim$y, stim$color, stim$duration, stim$responseWindow)

    return(simH.present(cdTodb(stim$level, .OpiEnv$SimH$maxStim), .OpiEnv$SimH$cap, fpr, fnr, tt, .OpiEnv$SimH$A, .OpiEnv$SimH$B))
}

########################################## TO DO !
simH.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH temporal persenter yet")
}

##################################################################
# Assumes static thresholds/FoS curves and static reaction times.
# Note that false positives and false negatives 
# have to be treated separately from the static responses.
# The location of a false positive is randomly drawn from any 
# location prior to the "true positive" point.
# Note FoS parameters and reaction times picked up 
# from OpiEnv$SimH set in opiInitialize
#
# NOTE Only works for single path vectors!
#
# @param tt - a list of functions that give true static threshold in dB
#             (relative to .OpiEnv$SimH$maxStim) as a function of distance along the path
#             tt(x)==NA implies never seen
# @param fpr - false positive rate in [0,1] (note for whole path)
# @param fnr - false negative rate in [0,1] (note for whole path)
# @param criteria - static probability of seeing where person presses
# @param rt_shape - response time drawn from rgamma(1, rt_shape, rt_rate) * rt_scale
# @param rt_rate  - response time drawn from rgamma(1, rt_shape, rt_rate) * rt_scale
# @param rt_scale - response time drawn from rgamma(1, rt_shape, rt_rate) * rt_scale
##################################################################
simH.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=NULL, 
                            criteria=0.95, rt_shape=5.3, rt_rate=1.4, rt_scale=0.1) {
    if (is.null(stim))
        stop("stim is NULL in call to opiPresent (using SimHensonRT, opiKineticStimulus)")
    if (!is.null(nextStim))
        stop("nextStim should be NULL for kinetic in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

    num_paths <- length(stim$path$x) - 1

    if (num_paths != 1) 
        stop("Sorry love; kinetic for SimHensonRT only works for single path vectors")

    if (is.null(tt))
        stop("tt must be a list of functions in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

    if (is(tt[[1]])[1] != "function")
        stop("tt must be a *list* of functions in call to opiPresent (using SimHensonRT, opiKineticStimulus)")

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
    # Find the first location along the paths where Pr seeing is at least criteria.
    # Assumes a FoS slope of Gauss(mean=min(6,exp(3.27 -0.081* tt)), stdev=0.25) at all locations.
    ############################################################################## 
    eDistP <- function(x1,y1,x2,y2) sqrt((x1-x2)^2 + (y1-y2)^2) 

    prSeeing <- function(stim, tt) {
        if (is.na(tt))
            return(0)

        #slope <- rnorm(1, mean=min(.OpiEnv$SimH$cap, exp(.OpiEnv$SimH$A*tt + .OpiEnv$SimH$B)), 0.25)
        slope <- min(.OpiEnv$SimH$cap, exp(.OpiEnv$SimH$A*tt + .OpiEnv$SimH$B))
        return(1 - pnorm(stim, tt, slope))
    }

    path_num <- 1   # for future when multi-paths are supported (bwahahahaha!)

    stim_db <- cdTodb(stim$levels[path_num], .OpiEnv$SimH$maxStim)
    
    path_len <- eDistP(stim$path$x[path_num], stim$path$y[path_num],
                       stim$path$x[path_num+1], stim$path$y[path_num+1])

    path_angle <- atan2(stim$path$y[path_num+1] - stim$path$y[path_num], stim$path$x[path_num+1] - stim$path$x[path_num])

        # give difference between prSeeing at dist_along_path and criteria
    f <- function(dist_along_path) prSeeing(stim_db, tt[[path_num]](dist_along_path))

    ds <- seq(0,path_len, 0.01)
    ii <- which(sapply(ds, f) >= criteria)

    seeing_point <- NULL
    if (length(ii) > 0)  # found!
        seeing_point <- list(distance_in_path=ds[head(ii,1)], path_num=path_num)

    #######################################################
    # Check for false repsonses. Randomise order to check.
    ######################################################
    fn_ret <- list(err=NULL, seen=FALSE, time=NA, x=NA, y=NA)

    if (is.null(seeing_point)) {
        max_d <- eDistP(stim$path$x[path_num], stim$path$y[path_num], stim$path$x[path_num+1], stim$path$y[path_num+1])
    } else {
        max_d <- seeing_point$distance_in_path
    }

    d <- runif(1, min=0, max=max_d)

    fp_ret <- list(err=NULL,
               seen=TRUE,
               time=d / stim$speeds[path_num] * 1000, 
               x=stim$path$x[path_num] + d*cos(path_angle),
               y=stim$path$y[path_num] + d*sin(path_angle)
           )

    if (runif(1) < 0.5) {
        if (runif(1) < fpr) return(fp_ret)   # fp first, then fn
        if (runif(1) < fnr) return(fn_ret)
    } else {
        if (runif(1) < fnr) return(fn_ret)   # fn first, then fp
        if (runif(1) < fpr) return(fp_ret)
    }

    #######################################################
    # Get to here, then no false response
    ######################################################
    if (is.null(seeing_point)) {
        return(list(err=NULL, seen=FALSE, time=NA, x=NA, y=NA))
    } else {
            #sample a reaction time
        rt <- rgamma(1, shape=rt_shape, rate=rt_rate) / rt_scale

        d <- seeing_point$distance_in_path + rt * stim$speeds[path_num] / 1000

        return(list(err=NULL,
                seen=TRUE,
                time=d / stim$speeds[path_num] * 1000,
                x=stim$path$x[path_num] + d*cos(path_angle),
                y=stim$path$y[path_num] + d*sin(path_angle)
        ))
    }
}

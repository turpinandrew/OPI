#
# An implementation of the OPI that simulates responses using 
# Henson et al (2000) variability.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: June 2012
#
# Modified  8 Jul 2014: added type="X" to opiInitialise and opiPresent
# Modified 20 Jul 2014: added maxStim argument for cdTodB conversion
# Modified Feburary 2017: Moved kinetic over from simH_RT.r
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
#' @rdname opiInitialize
#' @param type N|G|C for the three Henson params for "SimHenson", "SimHensonRT". Type 'X'
#'   to specify your own A and B values (eg different dB scale)
#' @param A parameter A for "SimHenson", "SimHensonRT"
#' @param B parameter B for "SimHenson", "SimHensonRT"
#' @param cap dB value for capping stdev form Henson formula for "SimHenson", "SimHensonRT"
#' @param display Dimensions of plot area (-x,+x,-y,+y) to display stim. No display if NULL.
#'   For "SimHenson", "SimHensonRT", "SimGaussian", "SimNo", "SimYes"
#' @param maxStim Maximum stimulus value in cd/m^2 used for db <-> cd/m^2 conversions for
#'   "SimHenson", "SimHensonRT", "SimGaussian"
#' @details
#' \subsection{SimHenson}{
#'   \code{opiInitialize(type="C", A=NA, B=NA, cap=6, maxStim=10000/pi)}
#'   
#'   If the chosen OPI implementation is \code{SimHenson}, then \code{type}
#'   can be one of: \code{"N"}, for normal patients; \code{"G"}, for POAG
#'   patients; and \code{"C"}, for a combination. See Table 1 in Henson et al
#'   (2000).
#'   
#'   If \code{type} is \code{"X"} then \code{A} and \code{B} should be
#'   specified and are used in place of one of the three A/B combinations as in
#'   Henson et al (2000). \code{cap} is the maximum standard deviation value that
#'   the simulator will use for the slope/spread of the psychometric function.
#'   
#'   If \code{display} is a vector of four numbers \code{c(xlow, xhi, ylow, yhi)},
#'   then a plot area is created of dimension \code{xlim=range(xlow, xhi)} and
#'   \code{ylim=range(ylow, yhi)} and each call to \code{opiPresent} will display
#'   a point on the area. The color of the plot area can be set with
#'   \code{opiSetBackground}, and the color of the displayed point is determined
#'   by the stimulus passed to \code{opiPresent}.
#'   
#'   \code{maxStim} is the maximum stimuls value in cd/\eqn{\mbox{m}^2}{m^2}.
#'   This is used in converting cd/\eqn{\mbox{m}^2}{m^2} to dB values, and
#'   vice versa.
#' }
#' @examples
#' # Set up a simple simulation for white-on-white perimetry
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#'
#' # Set up a simple simulation for white-on-white perimetry
#' # and display the stimuli in a plot region
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
simH.opiInitialize <- function(type = "C", A = -0.081, B = 3.27, cap = 6,
                               display = NA, maxStim = 10000 / pi) {
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
#' @rdname opiSetBackground
#' @param col DESCRIPTION for OPI implementations based on simulations
#' @param gridCol DESCRIPTION for OPI implementation based on simulations
#' @details
#' \subsection{SimHenson}{ 
#'   \code{opiSetBackground(col, gridCol)}
#'   
#'   \code{col} is the background color of the plot area used for displaying
#'   stimuli, and \code{gridCol} the color of the gridlines. Note the plot area
#'   will only be displayed if \code{opiInitialize} is called with a valid display
#'   argument.
#' }
simH.opiSetBackground <- function(col, gridCol) { 
    return (simDisplay.setBackground(col, gridCol))
}

#' @rdname opiPresent
#' @param fpr false positive rate for OPI implementation "SimHenson"
#' @param fnr false negative rate for OPI implementation "SimHenson"
#' @param tt  SOMETHING for OPI implementation "SimHenson"
#' @param criteria CRITERIA for OPI implementation "SimHenson"
#' @param rt_shape response time shape parameter for OPI implementation "SimHenson"
#' @param rt_rate response time rate parameter  for OPI implementation "SimHenson"
#' @param rt_scale response time scale parameter for OPI implementation "SimHenson"
#' @details
#' \subsection{SimHenson}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}
#'   
#'   If the chosen OPI implementation is \code{SimHenson}, then the response to a
#'   stimuli is determined by sampling from a Frequency-of-Seeing (FoS) curve
#'   (also known as the psychometric function) with formula
#'   \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt}},
#'   where \eqn{x}{\code{x}} is the stimulus value in Humphrey dB, and pxVar is
#'   \deqn{\min\left(\mbox{simH.global.cap}, e^{A\times\mbox{tt}+B}\right).}{\code{min(simH.cap, exp(A*tt + B))}.}
#'   The ceiling \code{simH.global.cap} is set with the call to
#'   \code{opiInitialize}, and \code{A} and \code{B} are from Table 1 in Henson
#'   et al (2000). Which values are used is determined by \code{simH.type} which
#'   is also set in the call to \code{opiInitialize}.
#'   
#'   Note that if the stimulus value is less than zero, then the Henson formula
#'   is not used. The probability of seeing is \code{fpr}.
#'   
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=NULL,
#'                    criteria=0.95, rt_shape=5.3, rt_rate=1.4, rt_scale=0.1)}
#'
#'   For determinng seen/not-seen for kinetic, the first location (to a fidelity
#'   of 0.01 degrees) on the path (it only works for single paths now) where the
#'   probability of seeing is equal to \code{criteria} is found. If no such
#'   location exists, then the stimuli is not seen. The probability of seeing at
#'   each location is determined using a frequency-of-seeing curve defined as a
#'   cumulative Gaussian with parameters controlled by \code{tt} and
#'   \code{opiInitialize}. At each location along the path, the mean of the FoS
#'   is taken from the \code{tt} function, which takes a distance-along-path
#'   (in degrees) as an argument, and returns a dB value which is the static
#'   threshold at that distance along the path.
#'   
#'   Function \code{tt} can return NA for not thresholds that are always not
#'   seen. At each location along the path, the standard deviation of the FoS
#'   is sampled from a Gaussion with mean taken from the formula of Henson et
#'   al (2000), as parametrised by \code{opiInitialize}, and standard deviation
#'   0.25.
#'   
#'   The location of a false positive response (for the total kinetic path) is
#'   sampled uniformly from the start of the path to the 'seeing' location, or
#'   the entire path if the stimuli is not seen.
#'   
#'   Note that the false positive rate \code{fpr} and the false negative rate
#'   \code{fnr} are specified for the whole path, and not for the individual
#'   static responses along the way.
#'   
#'   The actual location returned for a seen response is the location where the
#'   probability of seeing equals \code{criteria}, plus a response time sampled
#'   from a Gamma distribution parameterised by \code{rt_shape} and \code{rt_rate}
#'   and multiplied by \code{rt_scale}.That is:
#'   \code{rgamma(1, shape=rt_shape, rate=rt_rate) / rt_scale}.
#' }
#' @examples
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'   s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=0.43, color="white",
#'             duration=200, responseWindow=1500)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#' 
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#' 
#' result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)
#' 
#' # Will not work as 'stim' is not named
#' #result <- opiPresent(makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)
#' 
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
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

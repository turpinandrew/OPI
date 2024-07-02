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

if (exists(".OpiEnv") && !exists("SimHRT", where=.OpiEnv))
    assign("SimHRT", new.env(5), envir=.OpiEnv)

#' @title opiClose_for_SimHensonRT
#' @description Does nothing.
#' @usage NULL
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiClose_for_SimHensonRT <- function() list(err = NULL)

#' @title opiQueryDevice_for_SimHensonRT
#' @description
#' Returns name of the machine.
#' @usage NULL
#'
#' @return A list with elements:
#'   * \code{isSim} Always \code{TRUE}.
#'   * \code{machine} that is set to `"SimHensonRT"`.
#'
opiQueryDevice_for_SimHensonRT <- function() list(isSim = TRUE, machine = "SimHensonRT")

#' @title opiInitialise_for_SimHensonRT
#' @description
#' Simulates responses using a Frequency of Seeing (FoS) curve.
#'
#' For internal use only, use `opiInitialize()`.
#'
#' The FoS is modelled as a cumulative Gaussian function over dB with
#' standard deviation equal to `min(cap, exp( A * t + B))`, where
#' t is the threshold/mean of the FoS in dB.
#' All values are in dB relative to `maxStim`.
#'
#' @usage NULL
#'
#' @param type A single character that is:
#'   *  `N` for using the A and B values from the Normals in Henson et al (2000)
#'   *  `G` for using the A and B values from the Glaucomas in Henson et al (2000)
#'   *  `C` for using the A and B values from the Combined in Henson et al (2000)
#'   *  `X` to specify your own A and B values as parameters
#' @param A Coefficient of `t` in the formula (ignored if `type != 'X'`).
#' @param B Addend of `t` in the formula (ignored if `type != 'X'`).
#' @param cap Maximum dB value for the stdev of the FoS curve.
#' @param maxStim The maximum stimulus value (0 dB) in cd/\eqn{\mbox{m}^2}{m^2}.
#' @param rtData A data.frame with colnames == "Rt", "Dist", "Person" (or NULL for default).
#' @param rtFP A response time for false positives ??? for "SimHensonRT"
#' @param ... Any other parameters you like, they are ignored.
#
#' @details
#'   If the chosen OPI implementation is \code{SimHensonRT}, then the first six
#'   parameters are as in \code{SimHenson}, and \code{rtData} is a data frame
#'   with at least 2 columns: \code{"Rt"}, response time; and \code{"Dist"},
#'   signifying that distance between assumed threshold and stimulus value in
#'   your units.
#'
#'   This package contains \code{RtSigmaUnits} or \code{RtDbUnits} that can be
#'   loaded with the commands \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)},
#'   and are suitable to pass as values for \code{rtData}.
#'
#'   \code{rtFp} gives the vector of values in milliseconds from which a response
#'   time for a false positive response is randomly sampled.
#'
#' @examples
#' # Set up a simple simulation for white-on-white perimetry
#' # and display the stimuli in a plot region and simulate response times
#' chooseOpi("SimHensonRT")
#' data(RtSigmaUnits)
#' oi <- opiInitialize(type="C", cap=6, display=NA, rtData=RtSigmaUnits, rtFP=1:100)
#' if (!is.null(oi$err))
#'   stop("opiInitialize failed")
#'
#' @return A list with elements:
#'   * \code{err} NULL if initialised, string msg otherwise
#'
#' @examples
#'     # Set up a simple simulation for white-on-white perimetry
#' chooseOpi("SimHenson")
#' res <- opiInitialize(type = "C", cap = 6)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$err))
#'
opiInitialise_for_SimHensonRT <- function(type = "C", A = -0.081, B = 3.27, cap = 6, maxStim = 10000 / pi, rtData = NULL, rtFP = 1:1600, ...) {
    if (!is.element(type, c("N", "G", "C", "X"))) {
        msg <- paste("Bad 'type' specified for SimHenson in opiInitialize():", type)
        warning(msg)
        return(list(err = msg))
    }

    if (cap < 0)
        warning("cap is negative in call to opiInitialize (simHenson)")

    if (exists(".opi_env") && !exists("sim_henson", where = .opi_env))
        assign("sim_henson", new.env(), envir = .opi_env)

    assign("type",    type, envir = .opi_env$sim_henson)
    assign("cap",     cap, envir = .opi_env$sim_henson)
    assign("maxStim", maxStim, envir = .opi_env$sim_henson)

    if (type == "N") {
        assign("A", -0.066, envir = .opi_env$sim_henson)
        assign("B", 2.81, envir = .opi_env$sim_henson)
    } else if (type == "G") {
        assign("A", -0.098, envir = .opi_env$sim_henson)
        assign("B", 3.62, envir = .opi_env$sim_henson)
    } else if (type == "C") {
        assign("A", -0.081, envir = .opi_env$sim_henson)
        assign("B", 3.27, envir = .opi_env$sim_henson)
    } else if (type == "X") {
        assign("A", A, envir = .opi_env$sim_henson)
        assign("B", B, envir = .opi_env$sim_henson)
    }

    if (type == "X" && (is.na(A) || is.na(B))) {
        msg <- "opiInitialize (SimHenson): you have chosen type X, but one/both A and B are NA"
        warning(msg)
        return(list(err = msg))
    }

    if (is.null(rtData)) {
        msg <- "opiInitialize (SimHensonRT): rtData cannot be NULL"
        warning(msg)
        return(list(err = msg))
    }
    if (nrow(rtData) < 100)
        warning("opiInitialize (SimHensonRT): Less than 100 rows in rtData; that's wierd")
    if (ncol(rtData) != 3
        || !("Rt" %in% colnames(rtData))
        || !("Dist" %in% colnames(rtData))
        || !("Person" %in% colnames(rtData))) {
            msg <- "opiInitialize (SimHensonRT): rtData must have 3 columns: Rt, Dist, Person. See data(RtSigmaUnits) for example."
            warning(msg)
            return(list(err = msg))
    }
    assign("rtData", rtData, envir = .opi_env$sim_henson)

    if (length(rtFP) < 1) {
        msg <- "opiInitialize (SimHensonRT): rtFP must have at least 1 element"
        warning(msg)
        return(list(err = msg))
    }

    assign("rtFP", rtFP, envir = .opi_env$sim_henson)
print(paste(.opi_env$sim_henson$cap, .opi_env$sim_henson$A, .opi_env$sim_henson$B))

    return(list(err = NULL))
}

#' @title opiSetup_for_SimHensonRT
#' @description
#' Does nothing.
#' For internal use only, use `opiSetup()`.
#' @usage NULL
#'
#' @param ... Any object you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiSetup_for_SimHensonRT <- function(...) list(err = NULL)

#' @title opiPresent_for_SimHensonRT
#' @description
#' Determine the response to a stimuli by sampling from a cumulative Gaussian
#' Frequency-of-Seeing (FoS) curve (also known as the psychometric function).
#'
#' For internal use only, use `opiPresent()`.
#'
#' @usage NULL
#' @param stim A list that contains at least:
#'   * `level` which is the stim value in cd/\eqn{\mbox{m}^2}{m^2}.
#' @param fpr false positive rate for the FoS curve (range 0..1).
#' @param fnr false negative rate for the FoS curve (range 0..1).
#' @param tt  mean of the assumed FoS curve in dB.
#' @param dist The distance of the stimulus level from \code{tt} in appropriate units (same as \code{rtData$Dist}).
#' @param ...  Any other parameters you like, they are ignored.
#'
#' @details
#' As the response time returned for a false positive is determined
#' separately from a positive response, we first check for a false response.
#' If there is no false response, we use the FoS formula
#' \deqn{1-\mbox{pnorm}(x, \mbox{tt}, \mbox{pxVar})}
#' where `x` is the stimulus value in dB, and `pxVar` is
#' \deqn{\min(\mbox{cap}, e^{A\times\mbox{tt}+B}).}
#' The ceiling \code{cap} is set with the call to
#' \code{opiInitialize}, and \code{A} and \code{B} are from Table 1 in Henson
#' et al (2000), also set in the call to `opiInitialise` using the \code{type} parameter.
#'
#'   Thus, this function is the same as for \code{SimHenson}, but
#'   reaction times are determined by sampling from \code{rtData} as passed to
#'   \code{opiInitialize}.  The \code{dist} parameter is the distance of the
#'   stimulus level from the true threshold, and should be in the same units as
#'   the \code{Dist} column of \code{rtData}. The default is just the straight
#'   difference between the stimulus level and the true threshold, but you might
#'   want it scaled somehow to match \code{rtData}.
#'
#' @return A list with elements:
#'   * \code{err} \code{NULL} if no error, a string message otherwise.
#'   * \code{seen} \code{TRUE} or \code{FALSE}.
#'   * \code{time} The response time.
#'
#' @examples
#'     # Stimulus is Size III white-on-white as in the HFA
#' chooseOpi("SimHensonRt")
#' data(RtSigmaUnits)
#' res <- opiInitialize(type = "C", cap = 6, rtData = RtSigmaUnits)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$err))
#'
#' dist <- (10 - 30) / min(exp(-0.098 * 30 + 3.62), 6)
#' result <- opiPresent(stim = list(level = dbTocd(20)), tt = 30, fpr = 0.15, fnr = 0.01, dist=dist)
#' print(result, quote = FALSE)
#'
#' res <- opiClose()
#' if (!is.null(res$err))
#'   stop(paste("opiClose() failed:", res$err))
#'
#'
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
opiPresent_for_SimHensonRT <- function(stim, fpr = 0.03, fnr = 0.01, tt = 30, dist = 5, ...) {
    if (!exists(".opi_env") || !exists("sim_henson", where = .opi_env))
        return(list(err = "You have not called opiInitialise."))

    if (is.null(stim) || ! "level" %in% names(stim))
        return(list(err = "'stim' should be a list with a name 'level'. stim$level is the cd/m^2 to present."))

    if (stats::runif(1) < 0.5) { # check fpr then fnr
        if (stats::runif(1) < fpr)
            return(list(err = NULL, seen = TRUE, time = sample(.opi_env$sim_henson$rtFP, size = 1)))
        if (stats::runif(1) < fnr)
            return(list(err = NULL, seen = FALSE, time = NA))
    }  else { # check fnr then fpr
        if (stats::runif(1) < fnr)
            return(list(err = NULL, seen = FALSE, time = NA))
        if (stats::runif(1) < fpr)
            return(list(err = NULL, seen = TRUE, time = sample(.opi_env$sim_henson$rtFP, size = 1)))
    }

    level <- cdTodb(stim$level, .opi_env$sim_henson$maxStim)
    px_var <- min(.opi_env$sim_henson$cap, exp(.opi_env$sim_henson$A*tt + .opi_env$sim_henson$B)) # variability of patient, henson formula
    if (stats::runif(1) < 1 - stats::pnorm(level, mean = tt, sd = px_var)) {
        ds <- abs(.opi_env$sim_henson$rtData$Dist - dist)
        m <- utils::head(which.min(ds), 1)
        return(list(err = NULL, seen = TRUE, time = .opi_env$sim_henson$rtData$Rt[m]))
    } else {
        return(list(err = NULL, seen = FALSE, time = NA))
    }
}
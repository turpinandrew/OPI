
# An implementation of the OPI that simulates responses using
# Henson et al (2000) variability.
#
# Author: Andrew Turpin
#
# Copyright [2022] [Andrew Turpin & Ivan Marin-Franch]
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

#' @title opiClose_for_SimHenson
#' @description Does nothing.
#' @usage NULL
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiClose_for_SimHenson <- function() list(err = NULL)

#' @title opiQueryDevice_for_SimHenson
#' @description
#' Returns name of the machine.
#' @usage NULL
#'
#' @return A list with elements:
#'   * \code{isSim} Always \code{TRUE}.
#'   * \code{machine} that is set to `"SimHenson"`.
#'
opiQueryDevice_for_SimHenson <- function() list(isSim = TRUE, machine = "SimHenson")

#' @title opiInitialise_for_SimHenson
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
#' @param ... Any other parameters you like, they are ignored.
#'
#' @return A list with elements:
#'   * \code{err} NULL if initialised, msg otherwise
#'
#' @examples
#'     # Set up a simple simulation for white-on-white perimetry
#' chooseOpi("SimHenson")
#' res <- opiInitialize(type = "C", cap = 6)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$err))
#'
opiInitialise_for_SimHenson <- function(type = "C", A = -0.081, B = 3.27, cap = 6, maxStim = 10000 / pi, ...) {
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
        assign("A", -0.098 , envir = .opi_env$sim_henson)
        assign("B", 3.62, envir = .opi_env$sim_henson)
    } else if (type == "C") {
        assign("A", -0.081, envir = .opi_env$sim_henson)
        assign("B", 3.27, envir = .opi_env$sim_henson)
    } else if (type == "X") {
        assign("A" ,  A, envir = .opi_env$sim_henson)
        assign("B",  B, envir = .opi_env$sim_henson)
    }

    if (type == "X" && (is.na(A) || is.na(B))) {
        msg <- "opiInitialize (SimHenson): you have chosen type X, but one/both A and B are NA"
        warning(msg)
        return(list(err = msg))
    }

    return(list(err = NULL))
}

#' @title opiSetup_for_SimHenson
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
opiSetup_for_SimHenson <- function(...) list(err = NULL)

#' @title opiPresent_for_SimHenson
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
#' @param ...  Any other parameters you like, they are ignored.
#' 
#' @details
#' The FoS formula is
#' \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt}, \mbox{pxVar})}
#' where `x` is the stimulus value in dB, and `pxVar` is
#' \deqn{\min(\mbox{cap}, e^{A\times\mbox{tt}+B}).}
#' The ceiling \code{cap} is set with the call to
#' \code{opiInitialize}, and \code{A} and \code{B} are from Table 1 in Henson
#' et al (2000), also set in the call to `opiInitialise` using the \code{type} parameter.
#'
#' @return A list with elements:
#'   * \code{err} \code{NULL} if no error, a string message otherwise.
#'   * \code{seen} \code{TRUE} or \code{FALSE}.
#'   * \code{time} Always \code{NA}.
#'
#' @examples
#'     # Stimulus is Size III white-on-white as in the HFA
#' chooseOpi("SimHenson")
#' res <- opiInitialize(type = "C", cap = 6)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$err))
#'
#' result <- opiPresent(stim = list(level = dbTocd(20)), tt = 30, fpr = 0.15, fnr = 0.01)
#' print(paste("Seen:", result$seen, quote = FALSE))
#'
#' res <- opiClose()
#' if (!is.null(res$err))
#'   stop(paste("opiClose() failed:", res$err))
#'
opiPresent_for_SimHenson <- function(stim, fpr = 0.03, fnr = 0.01, tt = 30, ...) {
    if (!exists(".opi_env") || !exists("sim_henson", where = .opi_env))
        return(list(err = "You have not called opiInitialise."))

    if (is.null(stim) || ! "level" %in% names(stim))
        return(list(err = "'stim' should be a list with a name 'level'. stim$level is the cd/m^2 to present."))

    level <- cdTodb(stim$level, .opi_env$sim_henson$maxStim)
    px_var <- min(.opi_env$sim_henson$cap, exp(.opi_env$sim_henson$A*tt + .opi_env$sim_henson$B)) # variability of patient, henson formula 

    pr_seeing <- fpr + (1 - fpr - fnr) * (1 - stats::pnorm(level, mean = tt, sd = px_var))

    return(list(
        err = NULL,
        seen = stats::runif(1) < pr_seeing,
        time = NA
    ))
}

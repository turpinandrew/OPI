#
# An implementation of the OPI that simulates responses using
# a Cummulative Gaussian distribution as the Frequency of Seeing curve.
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

#' @title opiInitialize_for_SimGaussian
#' @description
#' Simulates responses using a Frequency of Seeing (FoS) curve.
#'
#' The FoS is modelled as a cumulative Gaussian function with standard deviation
#' equal to `sd` as provided and the mean as the true threshold given as `tt`
#' [opiPresent].
#' All values are in dB relative to `maxStim`.
#'
#' This is for internal use only, use [opiInitialize()].
#' @usage NULL
#'
#' @param sd Standard deviation of Cumulative Gaussian.
#' @param maxStim The maximum stimuls value (0 dB) in cd/\eqn{\mbox{m}^2}{m^2}.
#' @param ... Any other parameters you like, they are ignored.
#'
#' @return A list with elements:
#'   * \code{err} NULL if initialised, a message otherwise
#'
#' @examples
#'     # Set up a simple simulation for white-on-white perimetry
#' chooseOpi("SimGaussian")
#' res <- opiInitialize(sd = 2.5)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$msg))
#'
opiInitialise_for_SimGaussian <- function(sd = 1.0, maxStim = 10000 / pi, ...) {
    if (!is.numeric(sd) || (sd < 0)) {
        msg <- paste("Invalid standard deviation in opiInitialize for SimGaussian:", sd)
        warning(msg)
        return(list(err = msg))
    }

    if (exists(".opi_env") && !exists("sim_gaussian", where = .opi_env))
        assign("sim_gaussian", new.env(), envir = .opi_env)

    assign("sd",      sd,      envir = .opi_env$sim_gaussian)
    assign("maxStim", maxStim, envir = .opi_env$sim_gaussian)

    assign("machine_is_initialised", TRUE, .opi_env)
    return(list(err = NULL))
}

#' @title opiPresent_for_SimGaussian
#' @description
#' Determine the response to a stimuli by sampling from a cumulative Gaussian
#' Frequency-of-Seeing (FoS) curve (also known as the psychometric function).
#'
#' The FoS has formula
#' \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt}, \mbox{sd})}
#' where \eqn{x}{\code{x}} is the stimulus value in dB, and `sd` is
#' set by \code{opiInitialize} and \code{tt}, \code{fpr} and \code{fnr}
#' are parameters.
#'
#' This is for internal use only, use [opiPresent()].
#'
#' @usage NULL
#'
#' @param stim A list that contains at least:
#'   * `level` which is the stim value in cd/\eqn{\mbox{m}^2}{m^2}.
#' @param fpr false positive rate for the FoS curve (range 0..1).
#' @param fnr false negative rate for the FoS curve (range 0..1).
#' @param tt  mean of the assumed FoS curve in dB.
#' @param ...  Any other parameters you like, they are ignored.
#'
#' @return A list with elements:
#'   * \code{err} \code{NULL} if no error, a string message otherwise.
#'   * \code{seen} \code{TRUE} or \code{FALSE}.
#'   * \code{time} Always \code{NA}.
#'
#' @examples
#'     # Stimulus is Size III white-on-white as in the HFA
#' chooseOpi("SimGaussian")
#' res <- opiInitialize(sd = 1.6)
#' if (!is.null(res$err))
#'   stop(paste("opiInitialize() failed:", res$err))
#'
#' result <- opiPresent(stim = list(level = dbTocd(20)), tt = 30, fpr = 0.15, fnr = 0.01)
#' print(paste("Seen:", result$seen, quote = FALSE))
#'
#' res <- opiClose()
#' if (!is.null(res$err))
#'   warning(paste("opiClose() failed:", res$err))
#'
opiPresent_for_SimGaussian <- function(stim, fpr = 0.03, fnr = 0.01, tt = 30, ...) {
    if (!exists(".opi_env") || !exists("sim_gaussian", where = .opi_env))
        return(list(err = "You have not called opiInitialise."))

    if (is.null(stim) || ! "level" %in% names(stim))
        return(list(err = "'stim' should be a list with a name 'level'. stim$level is the cd/m^2 to present."))

    px_var <- .opi_env$sim_gaussian$sd

    level <- cdTodb(stim$level, .opi_env$sim_gaussian$maxStim)

    pr_seeing <- fpr + (1 - fpr - fnr) * (1 - stats::pnorm(level, mean = tt, sd = px_var))

    return(list(
        err = NULL,
        seen = stats::runif(1) < pr_seeing,
        time = NA
    ))
}

#' @title opiClose_for_SimGaussian
#' @description Does nothing.
#' @usage NULL
#'
#' @return A list with elements:
#'   * \code{error} Always \code{FALSE}.
#'   * \code{msg} A string "Close OK".
#'
opiClose_for_SimGaussian <- function() list(err = NULL)

#' @title opiQueryDevice_for_SimGaussian
#' @description Returns a simple list.
#' @usage NULL
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}
#'   * \code{machine} that is set to `"SimGaussian"`.
#'
opiQueryDevice_for_SimGaussian <- function() list(isSim = TRUE, machine = "SimGaussian")

#' @title opiSetup_for_SimGaussian
#' @description Does nothing.
#'
#' @usage NULL
#' @param ... Any object you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiSetup_for_SimGaussian <- function(...) list(err = NULL)

#
# 4-2 staircase with 2 reversals.
# See comments in full_threshold.r.
# This does not initiate a second staircase.
# This returns avenge of last two presentations as threshold.
#
# Includes
#     fourTwo.start    # initialise list state
#     fourTwo.step     # take state, present stim, update and return state
#     fourTwo.stop     # boolean - true if state is finished
#     fourTwo.final    # return final estimate from state
#
# Author: Andrew Turpin
# Date: December 2014
# Modified Tue 21 Mar 2023: changed licence from gnu to Apache 2.0
#
# Copyright [2022] [Andrew Turpin, Ivan Marin-Franch]
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
#

#' @rdname fourTwo
#' @title 4-2 Staircase
#' @description fourTwo is a 4-2 dB staircase beginning at level \code{est}
#' terminating after two reversals. The final estimate is the average of the
#' last two presentations. It also terminates if the \code{minStimulus} is
#' not seen twice, or the \code{maxStimulus} is seen twice.
#' @param est Starting estimate in dB
#' @param instRange Dynamic range of the instrument c(min,max) in dB
#' @param verbose True if you want each presentation printed
#' @param makeStim A function that takes a dB value and numPresentations
#' and returns an OPI datatype ready for passing to opiPresent
#' @param ... Extra parameters to pass to the opiPresent function
#' @details This is an implementation of a 4-2 1-up 1-down staircase. The
#' initial staircase starts at \code{est} and proceeds in steps of 4 dB until
#' the first reversal, and 2dB until the next reversal. The mean of the last
#' two presentations is taken as the threshold value. Note this function will
#' repeatedly call \code{opiPresent} for a stimulus until \code{opiPresent}
#' returns \code{NULL} (ie no error occurred). If more than one fourTwo is to
#' be interleaved (for example, testing multiple locations), then the
#' \code{fourTwo.start}, \code{fourTwo.step}, \code{fourTwo.stop} and
#' \code{fourTwo.final} calls can maintain the state of the fourTwo after
#' each presentation, and should be used. See examples below.
#' @return
#' \subsection{Multiple locations}{
#'   \code{fourTwo.start} returns a list that can be passed to \code{fourTwo.step},
#'   \code{fourTwo.stop}, and \code{fourTwo.final}. It represents the state of a fourTwo
#'   at a single location at a point in time and contains the following.
#'   \itemize{
#'     \item{name:}{\code{ fourTwo}}
#'     \item{}{ A copy of all of the parameters supplied to fourTwo.start:
#'       \code{startingEstimate=est}, \code{minStimulus=instRange[1]},
#'       \code{maxStimulus=instRange[2]}, \code{makeStim}, and \code{opiParams=list(...)}}
#'     \item{currentLevel:}{ The next stimulus to present.}
#'     \item{lastSeen:}{ The last seen stimulus.}
#'     \item{lastResponse:}{ The last response given.}
#'     \item{stairResult:}{ The final result if finished (initially \code{NA}).}
#'     \item{finished:}{ \code{"Not"} if staircase has not finished, or one of
#'       \code{"Rev"} (finished due to 2 reversals), \code{"Max"} (finished due to 2
#'       \code{maxStimulus} seen), \code{"Min"} (finished due to 2 \code{minStimulus} not seen)}
#'     \item{numberOfReversals:}{ Number of reversals so far.}
#'     \item{currSeenLimit:}{ Number of times \code{maxStimulus} has been seen.}
#'     \item{currNotSeenLimit:}{ Number of times \code{minStimulus} not seen.}
#'     \item{numPresentations:}{ Number of presentations so far.}
#'     \item{stimuli:}{ Vector of stimuli shown at each call to \code{fourTwo.step}.}
#'     \item{responses:}{ Vector of responses received (1 seen, 0 not) received at each call to \code{fourTwo.step}.}
#'     \item{responseTimes:}{ Vector of response times received at each call to \code{fourTwo.step}.}
#'   }
#'   \code{fourTwo.step} returns a list containing
#'   \itemize{
#'     \item{state:}{ The new state after presenting a stimuli and getting a response.}
#'     \item{resp:}{ The return from the \code{opiPresent} call that was made.}
#'   }
#'   \code{fourTwo.stop} returns \code{TRUE} if the staircase is finished (2 reversals, or \code{maxStimulus}
#'     is seen twice or \code{minStimulus} is not seen twice).
#'
#'   \code{fourTwo.final} returns the final estimate of threshold (mean of last
#'     two reversals). This issues a warning if called before the staircase has
#'     finished, but still returns a value.
#' }
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}, \code{\link{FT}}
#' @examples
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)$err))
#'   stop("opiInitialize failed")
#'
#' ##############################################
#' # This section is for multiple fourTwos
#' ##############################################
#' makeStimHelper <- function(db,n, x, y) {  # returns a function of (db,n)
#'   ff <- function(db, n) db+n
#'   body(ff) <- substitute({
#'     s <- list(x=x, y=y, level=dbTocd(db), size=0.43, color="white",
#'               duration=200, responseWindow=1500)
#'     class(s) <- "opiStaticStimulus"
#'     return(s)}, list(x=x,y=y))
#'   return(ff)
#' }
#' # List of (x, y, true threshold) triples
#' locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))
#'
#' # Setup starting states for each location
#' states <- lapply(locations, function(loc) {
#'   fourTwo.start(makeStim=makeStimHelper(db,n,loc[1],loc[2]),
#'                 tt=loc[3], fpr=0.03, fnr=0.01)})
#'
#' # Loop through until all states are "stop"
#' while(!all(st <- unlist(lapply(states, fourTwo.stop)))) {
#'   i <- which(!st)                         # choose a random,
#'   i <- i[runif(1, min=1, max=length(i))]  # unstopped state
#'   r <- fourTwo.step(states[[i]])               # step it
#'   states[[i]] <- r$state                  # update the states
#' }
#'
#' finals <- lapply(states, fourTwo.final)    # get final estimates of threshold
#' for(i in 1:length(locations)) {
#'   cat(sprintf("Location (%+2d,%+2d) ",locations[[i]][1], locations[[i]][2]))
#'       cat(sprintf("has threshold %4.2f\n", finals[[i]]))
#' }
#'
#' if (!is.null(opiClose()$err))
#'   warning("opiClose() failed")
#' @export
fourTwo.start <- function(est = 25, instRange = c(0,40), verbose = FALSE, makeStim, ...) {
    if (est < instRange[1] || est > instRange[2])
        stop("fourTwo.start: est must be in the range of instRange")

    return(list(name = "fourTwo",
        startingEstimate = est,
        currentLevel = est,
        minStimulus = instRange[1],
        maxStimulus = instRange[2],
        makeStim = makeStim,
        lastSeen = NA,
        lastResponse = NA,
        stairResult = NA,
        finished = "Not",                     # "Not", or one of "Max", "Min", "Rev"
        verbose = verbose,
        numberOfReversals = 0,
        currSeenLimit = 0,                    # number of times maxStimulus seen
        currNotSeenLimit = 0,                 # number of times minStimulus not seen
        numPresentations = 0,                 # number of presentations so far
        stimuli = NULL,                       # vector of stims shown
        responses = NULL,                     # vector of responses (1 seen, 0 not)
        responseTimes = NULL,                 # vector of response times
        opiParams = list(...)                 # the extra params
    ))
}# fourTwo.start()

#' @rdname fourTwo
#' @param state Current state of the fourTwo returned by
#' \code{fourTwo.start} and \code{fourTwo.step}
#' @param nextStim A valid object for \code{opiPresent} to
#' use as its \code{nextStim}.
#' @export
fourTwo.step <- function(state, nextStim = NULL) {
    if (state$finished != "Not")
        warning("fourTwo.step: stepping fourTwo staircase when it has already terminated")
    if (is.null(state$opiParams))
        params <- list(stim = state$makeStim(state$currentLevel, state$numPresentations), nextStim = nextStim)
    else
        params <- c(list(stim = state$makeStim(state$currentLevel, state$numPresentations), nextStim = nextStim), state$opiParams)
    opiResp <- do.call(opiPresent, params)
    while (!is.null(opiResp$err))
        opiResp <- do.call(opiPresent, params)

    state$stimuli          <- c(state$stimuli, state$currentLevel)
    state$responses        <- c(state$responses, opiResp$seen)
    state$responseTimes    <- c(state$responseTimes, opiResp$time)
    state$numPresentations <- state$numPresentations + 1

    if (state$verbose) {
        cat(sprintf("Presentation %2d: ", state$numPresentations))
        cat(sprintf("dB= %2d repsonse=%s\n", state$currentLevel, opiResp$seen))
    }

    if (opiResp$seen)
        state$lastSeen <- state$currentLevel

        # check for seeing min
    if (state$currentLevel == state$minStimulus && !opiResp$seen)
        state$currNotSeenLimit <- state$currNotSeenLimit + 1

        # check for seeing max
    if (state$currentLevel == state$maxStimulus && opiResp$seen)
        state$currSeenLimit <- state$currSeenLimit + 1

        # check for reversals
    if (state$numPresentations > 1 && opiResp$seen != state$lastResponse)
        state$numberOfReversals <- state$numberOfReversals + 1

    state$lastResponse <- opiResp$seen

        # check if staircase is finished.
    if (state$numberOfReversals >= 2) {
        state$finished <- "Rev"
        state$stairResult <- mean(utils::tail(state$stimuli, 2)) # mean of last two
    } else if (state$currNotSeenLimit >= 2) {
        state$finished <- "Min"
        state$stairResult <- state$minStimulus
    } else if (state$currSeenLimit >= 2) {
        state$finished <- "Max"
        state$stairResult <- state$maxStimulus
    } else {
            # update stimulus for next presentation
        delta <- ifelse(state$numberOfReversals == 0, 4, 2) * ifelse(opiResp$seen, +1, -1)
        state$currentLevel <- min(state$maxStimulus, max(state$minStimulus, state$currentLevel + delta))
    }

    return(list(state = state, resp = opiResp))
}#fourTwo.step()

#' @rdname fourTwo
#' @export
fourTwo.stop <- function(state) { return(state$finished != "Not") }

#' @rdname fourTwo
#' @export
fourTwo.final <- function(state) {
    if (state$finished != "Not")
        return (state$stairResult)
    else {
        warning("fourTwo.step: asking for final result of unfinished staircase")
        return(NA)
    }
}

########
# Test
###
### require(OPI)
### chooseOpi("SimHenson")
### #chooseOpi("SimYes")
### #chooseOpi("SimNo")
### opiInitialize()
### makeStim <- function(db, n) {
###     s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
###              duration=1200, responseWindow=500)
###     class(s) <- "opiStaticStimulus"
###
###     return(s)
### }
###
### res <- lapply(0:40, function(tt) {
###     lapply(1:1000, function(i) {
###         s <- fourTwo.start(makeStim=makeStim, tt=tt, fpr=0.15, fnr=0.3)
###         s <- fourTwo.step(s)
###         while(!fourTwo.stop(s$state)) {
###             s <- fourTwo.step(s$state)
###         }
###         fourTwo.final(s$state)
###     })
### })

#
# Full Threshold (FT) algorithm for a single location.
#
# Includes
#     FT          # Do a single 4-2 staircase
#     FT.start    # initialise list state
#     FT.step     # take state, present stim, update and return state
#     FT.stop     # boolean - true if state is finished
#     FT.final    # return final estimate from state
#
# FT begins with a 4-2dB staircase at level est. If the final estimate
# (last seen) is more than 4dB away from est, a second 4-2 staircase is
# completed beginning at the estimate returned from the first.
# There is special handling of the brightest and dimmest stimuli: see the
# comments in doStair() below.
#
# Based on Andrew Turpin's implementation in his Barramundi Simulator.
#
# Author: Andrew Turpin
#         Jonathan Denniss
# Date: June 2012
# Modified Tue 21 Mar 2023: changed licence from gnu to Apache 2.0
#
# Copyright [2022] [Andrew Turpin, Ivan Marin-Franch, Jonathan Denniss]
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

#' @rdname FT
#' @title Full Threshold
#' @description FT begins with a 4-2dB staircase beginning at level
#' \code{est}. If the final estimate (last seen) is more than 4dB away
#' from \code{est}, a second 4-2 staircase is completed beginning at the
#' estimate returned from the first
#' @param est Starting estimate in dB
#' @param instRange Dynamic range of the instrument c(min,max) in dB
#' @param verbose True if you want each presentation printed
#' @param makeStim A function that takes a dB value and numPresentations
#' and returns an OPI datatype ready for passing to opiPresent
#' @param ... Extra parameters to pass to the opiPresent function
#' @details This is an implementation of a 4-2 1-up 1-down staircase as
#' implemented in the first Humphrey Field Analyzer. The initial staircase
#' starts at \code{est} and proceeds in steps of 4 dB until the first
#' reversal, and 2dB until the next reversal. The last seen stimulus is
#' taken as the threshold value. If, after the first staircase, the
#' threshold is more than 4 dB away from the starting point, then a second
#' staircase is initiated with a starting point equal to the threshold
#' found with the first staircase.
#'
#' Note this function will repeatedly call \code{opiPresent} for a stimulus
#' until \code{opiPresent} returns \code{NULL} (ie no error occured)
#'
#' If more than one FT is to be interleaved (for example, testing multiple
#' locations), then the \code{FT.start}, \code{FT.step}, \code{FT.stop}
#' and \code{FT.final} calls can maintain the state of the FT after each
#' presentation, and should be used. If only a single FT is required, then
#' the simpler \code{FT} can be used. See examples below
#' @return
#' ## Single location
#'   Returns a list containing
#'     * \code{npres}, total number of presentations.
#'     * \code{respSeq}, response sequence stored as a list of (seen,dB) pairs.
#'     * \code{first}, first staircase estimate in dB.
#'     * \code{final}, final threshold estimate in dB.
#' 
#' ## Multilple locations
#'   \code{FT.start} returns a list that can be passed to \code{FT.step},
#'   \code{FT.stop}, and \code{FT.final}. It represents the state of a FT
#'     at a single location at a point in time and contains the following.
#'     * \code{name}, \code{FT}.
#'     * \code{startingEstimate=est}, input param.
#'     * \code{currentLevel}, the next stimulus to present.
#'     * \code{minStimulus=instRange[1]}, input param.
#'     * \code{maxStimulus=instRange[2]}, input param.
#'     * \code{makeStim}, input param.
#'     * \code{lastSeen}, the last seen stimulus.
#'     * \code{lastResponse}, the last response given.
#'     * \code{stairResult}, The final result if finished (initially \code{NA}).
#'     * \code{finished}, \code{"Not"} if staircase has not finished, or one of 
#'                      \code{"Rev"} (finished due to 2 reversals), 
#'                      \code{"Max"} (finished due to 2 \code{maxStimulus} seen), 
#'                      \code{"Min"} (finished due to 2 \code{minStimulus} not seen).
#'     * \code{verbose}, number of reversals so far.
#'     * \code{numberOfReversals}, number of reversals so far.
#'     * \code{currSeenLimit}, number of times \code{maxStimulus} has been seen.
#'     * \code{currNotSeenLimit}, number of times \code{minStimulus} not seen.
#'     * \code{numPresentations}, number of presentations so far.
#'     * \code{stimuli}, vector of stimuli shown at each call to \code{FT.step}.
#'     * \code{responses}, vector of responses received (1 seen, 0 not) received at each call to \code{FT.step}.
#'     * \code{responseTimes}, vector of response times received at each call to \code{FT.step}.
#'     * \code{opiParams=list(...)}, input param
#'     * \code{finished}, \code{TRUE} if staircase has finished (2 reversals, or max/min seen/not-seen twice).
#' 
#' \code{FT.step} returns a list containing
#'   * \code{state}, the new state after presenting a stimuli and getting a response.
#'   * \code{resp}, the return from the \code{opiPresent} call that was made.
#' 
#' \code{FT.stop} returns \code{TRUE} if the first staircase has had 2 reversals, or
#' \code{maxStimulus} is seen twice or \code{minStimulus} is not seen twice and the
#'   final estimate is within 4 dB of the starting stimulus. Returns \code{TRUE} if
#'   the second staircase has had 2 reversals, or \code{maxStimulus} is seen twice or
#'   \code{minStimulus} is not seen twice
#'
#' \code{FT.final} returns the final estimate of threshold based on state, which is
#'   the last seen in the second staircase, if it ran, or the first staircase otherwise
#'
#' \code{FT.final.details} returns a list containing
#'   * \code{final}, the final threshold.
#'   * \code{first}, the threshold determined by the first staircase (might be different from final).
#'   * \code{stopReason}, either \code{Reversals}, \code{Max}, or \code{Min} which are the three ways in which FT can terminate.
#'   * \code{np}, number of presentation for the whole procedure (including both staircases if run).
#' 
#' @references
#' A. Turpin, P.H. Artes and A.M. McKendrick. "The Open Perimetry
#' Interface: An enabling tool for clinical visual psychophysics", Journal
#' of Vision 12(11) 2012.
#'
#' H. Bebie, F. Fankhauser and J. Spahr. "Static perimetry: strategies",
#' Acta Ophthalmology 54 1976.
#'
#' C.A. Johnson, B.C. Chauhan, and L.R. Shapiro. "Properties of staircase
#' procedures for estimating thresholds in automated perimetry",
#' Investagative Ophthalmology and Vision Science 33 1993.
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}, \code{\link{fourTwo.start}}
#' @examples
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#'
#' result <- FT(makeStim=makeStim, tt=30, fpr=0.15, fnr=0.01)
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
#'
#' ##############################################
#' # This section is for multiple FTs
#' ##############################################
#' makeStimHelper <- function(db,n, x, y) {  # returns a function of (db,n)
#'   ff <- function(db, n) db+n
#'   body(ff) <- substitute({
#'     s <- list(x=x, y=y, level=dbTocd(db), size=0.43, color="white",
#'               duration=200, responseWindow=1500)
#'     class(s) <- "opiStaticStimulus"
#'     return(s)
#'   }, list(x=x,y=y))
#'   return(ff)
#' }
#'
#' # List of (x, y, true threshold) triples
#' locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))
#' # Setup starting states for each location
#' states <- lapply(locations, function(loc) {
#'   FT.start(makeStim=makeStimHelper(db,n,loc[1],loc[2]),
#'            tt=loc[3], fpr=0.03, fnr=0.01)})
#'
#' # Loop through until all states are "stop"
#' while(!all(st <- unlist(lapply(states, FT.stop)))) {
#'   i <- which(!st)                         # choose a random,
#'   i <- i[runif(1, min=1, max=length(i))]  # unstopped state
#'   r <- FT.step(states[[i]])               # step it
#'   states[[i]] <- r$state                  # update the states
#' }
#'
#' finals <- lapply(states, FT.final)    # get final estimates of threshold
#' for(i in 1:length(locations)) {
#'   cat(sprintf("Location (%+2d,%+2d) ",locations[[i]][1], locations[[i]][2]))
#'   cat(sprintf("has threshold %4.2f\n", finals[[i]]))
#' }
#'
#' if(!is.null(opiClose()))
#'   warning("opiClose() failed")
#' @export
FT <- function(est=25, instRange=c(0,40), verbose=FALSE, makeStim, ...) {
    #
    # Do a single 4-2 staircase beginning at startEstimate
    # and stopping after 2 reversals, or if min/max not/seen twice.
    # Return the reason for stopping, the last seen (or min/max)
    # and responseSequence
    #
    doStair <- function(startEstimate) {
        numRevs       <- 0    # number of reversals
        numPres       <- 0    # number of presentations
        numSeenMax    <- 0    # number of times seen instRange[2]
        numNotSeenMin <- 0    # number of times not seen instRange[1]
        lastSeen      <- NA   # last stimulus seen
        responseSeq   <- NULL # a list of (seen/not, db value) pairs

        currentEst <- startEstimate
        while (numRevs < 2 && numNotSeenMin < 2 && numSeenMax < 2) {
            opiResp <- opiPresent(stim=makeStim(currentEst, numPres), nextStim=NULL, ...)
            while (!is.null(opiResp$err))
                opiResp <- opiPresent(stim=makeStim(currentEst, numPres), nextStim=NULL, ...)
            resp <- opiResp$seen
            numPres <- numPres + 1

            if (verbose) {
                cat(sprintf("Presentation %2d: ", numPres))
                cat(sprintf("dB= %2d repsonse=%s\n", currentEst, resp))
            }

            responseSeq <- c(responseSeq, list(c(seen=resp, db=currentEst)))

            if (resp)
                lastSeen <- currentEst

            if (currentEst == instRange[1] && !resp)
                numNotSeenMin <- numNotSeenMin + 1

            if (currentEst == instRange[2] && resp)
                numSeenMax <- numSeenMax + 1

            if (numPres > 1 && resp != responseSeq[[numPres-1]]["seen"])
                numRevs <-numRevs + 1

            delta <- ifelse(numRevs == 0, 4, 2) * ifelse(resp, +1, -1)
            currentEst <- min(instRange[2],
                            max(instRange[1], currentEst + delta))
        }

        if (numSeenMax == 2) {
            final <- instRange[2]
            stopReason <- "Max"
        } else if (numNotSeenMin == 2) {
            stopReason <- "Min"
            final <- instRange[1]
        } else {
            stopReason <- "Reversals"
            final <- lastSeen
        }

        return (list(
            stopReason=stopReason,     # Reason for terminating staircase
            final=final,               # The threshold estimate in dB
            responseSeq=responseSeq    # A list of (seen, db) pairs
        ))
    }# doStair()

        #
        # do the first 4-2 stair and store the result
        #
    first <- doStair(est)
    final           <- first$final
    fullResponseSeq <- first$responseSeq

    if (first$stopReason == "Reversals" && abs(est - first$final) > 4) {
        second <- doStair(first$final)
        fullResponseSeq <- c(first$responseSeq, second$responseSeq)
        final <- second$final
    }

    return(list(
        npres=length(fullResponseSeq),  # number of presentations
        respSeq=fullResponseSeq,        # reposnse sequence (list of pairs)
        first=first$final,              # estimate from first staircase
        final=final                     # final threshold estimate
    ))
}#FT()

#' @rdname FT
#' @export
FT.start <- function(est=25, instRange=c(0,40), makeStim, ...) {
    if (est < instRange[1] || est > instRange[2])
        stop("FT.start: est must be in the range of instRange")

    return(list(name="FT",
                startingEstimate=est,
                currentLevel=est,
                minStimulus=instRange[1],
                maxStimulus=instRange[2],
                makeStim=makeStim,
                lastSeen=NA,
                lastResponse=NA,
                firstStairResult=NA,
                secondStairResult=NA,
                finished=FALSE,                     # flag to say it is finished
                numberOfReversals=0,
                currSeenLimit=0,                    # number of times maxStimulus seen
                currNotSeenLimit=0,                 # number of times minStimulus not seen
                numPresentations=0,                 # number of presentations so far
                stimuli=NULL,                       # vector of stims shown
                responses=NULL,                     # vector of responses (1 seen, 0 not)
                responseTimes=NULL,                 # vector of response times
                opiParams=list(...)                 # the extra params
    ))
}# FT.start()

#' @rdname FT
#' @param state Current state of the FT returned by \code{FT.start} and
#' \code{FT.step}
#' @param nextStim A valid object for \code{opiPresent} to use as its
#' \code{nextStim}
#' @export
FT.step <- function(state, nextStim=NULL) {
    if (is.null(state$opiParams))
        params <- list(stim=state$makeStim(state$currentLevel, state$numPresentations), nextStim=nextStim)
    else
        params <- c(list(stim=state$makeStim(state$currentLevel, state$numPresentations), nextStim=nextStim), state$opiParams)
    opiResp <- do.call(opiPresent, params)
    while (!is.null(opiResp$err))
        opiResp <- do.call(opiPresent, params)

    state$stimuli          <- c(state$stimuli, state$currentLevel)
    state$responses        <- c(state$responses, opiResp$seen)
    state$responseTimes    <- c(state$responseTimes, opiResp$time)
    state$numPresentations <- state$numPresentations + 1

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
    # If it is, and it is the first,
    # check if we need a second staircase
    thisStairStops <- state$numberOfReversals >= 2 || state$currNotSeenLimit >= 2 || state$currSeenLimit >= 2
    if (!thisStairStops) {
        # update stimulus for next presentation
        delta <- ifelse(state$numberOfReversals == 0, 4, 2) * ifelse(opiResp$seen, +1, -1)
        state$currentLevel <- min(state$maxStimulus, max(state$minStimulus, state$currentLevel + delta))
    } else {
        if (is.na(state$firstStairResult)) {
            first <- FT.final.details(state)
            state$firstStairResult <- first$final
            if (first$stopReason == "Reversals" && abs(state$firstStairResult - state$startingEstimate) > 4) {
                # initiate second staircase
                state$currentLevel      <- first$final
                state$currSeenLimit     <- 0
                state$currNotSeenLimit  <- 0
                state$numberOfReversals <- 0
                state$lastSeen          <- NA
            } else {
                state$finished <- TRUE
            }
        } else if (is.na(state$secondStairResult)) {
            state$secondStairResult <- state$lastSeen
            state$finished <- TRUE
        } else {
            warning("FT.step: stepping FT staircase when it has already terminated")
        }
    }

    return(list(state=state, resp=opiResp))
}#FT.step()

#' @rdname FT
#' @export
FT.stop <- function(state) { return(state$finished) }

FT.final.details <- function(state) {
    if (state$currSeenLimit == 2) {
        final <- state$maxStimulus
        stopReason <- "Max"
    } else if (state$currNotSeenLimit == 2) {
        stopReason <- "Min"
        final <- state$minStimulus
    } else {
        stopReason <- "Reversals"
        final <- state$lastSeen
    }

    return (list(
        final=final,                  # The threshold estimate in dB
        first=state$firstStairResult, # The threshold estimate in dB
        stopReason=stopReason,        # Reason for terminating staircase
        np=state$numPresentations
    ))
}

#' @rdname FT
#' @export
FT.final <- function(state) {
    return (FT.final.details(state)[["final"]])
}

########
# TEst

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
###         s <- FT.start(makeStim=makeStim, tt=tt, fpr=0.15, fnr=0.3)
###         s <- FT.step(s)
###         while(!FT.stop(s$state)) {
###             s <- FT.step(s$state)
###         }
###         FT.final(s$state)
###     })
### })
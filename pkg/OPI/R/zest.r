#
# ZEST algorithm that maintains state in a list - good for interleaved.
# Includes
#     ZEST          # just for a single location
#     ZEST.start    # initialise list state
#     ZEST.step     # take state, present stim, update and return state
#     ZEST.stop     # boolean - true if state is finished
#     ZEST.final    # return final estimate from state
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: August 2012
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

##########################
# little helper functions
##########################
ZEST.stdev <- function(state) { sqrt(sum(state$pdf*state$domain*state$domain) - sum(state$pdf * state$domain)^2) }

ZEST.entropy <- function(state) {
    z <- which(state$pdf > 0)
    return(-sum(state$pdf[z] * log2(state$pdf[z])))
}

################################################################################
# ZEST for a single location. 
#
# Input parameters
#   domain        List of dB values over which pdf is kept
#   prior         Probability distribution over domain.
#   likelihood    matrix where likelihood[s,t] is likelihood of seeing s given t is true thresh (Pr(s|t)
#                 where s and t are indexs into domain
#   stopType      N | S | H
#   stopValue     Value for num prs (N), stdev (S) of Entropy (H)
#   minNotSeenLimit Will terminate if lowest domain value not seen this many times
#   maxSeenLimit    Will terminate if highest domain value seen this many times
#   maxPresentations Maximum number of presentations
#   verbose       1 if you want pdfs returned, 2 is 1+print, 0 for none
#   makeStim      A helper function to create the required
#                 OPI data type for passing to opiPresent. 
#                 Can include checkFixationOK function in the returned stim.
#   stimChoice    "mean", "median", "mode"
#   ...           Parameters for opiPresent
# Returns a list containing
#   npres    Total number of presentations
#   respSeq  Response sequence stored as a matrix: row 1 = dB, row 2 = response 1/0
#   pdfs     Sequence of pdfs used (if verbose)
#
# Note 
#   1) stims are rounded to nearest domain entry 
#   2) opiPresent called infinitely until no error
################################################################################
#' @rdname ZEST
#' @title ZEST
#' @description An implementation of the Bayesian test procedures of King-Smith et al.
#' and Watson and Pelli. Note that we use the term \code{pdf} throughout as in the
#' original paper, even though they are discrete probability functions in this
#' implementation.
#' @param domain Vector of values over which pdf is kept.
#' @param prior Starting probability distribution over domain. Same length as \code{domain}.
#' @param likelihood Matrix where \code{likelihood[s,t]} is likelihood of seeing \code{s}
#'   given \code{t} is the true threshold. That is, Pr(s|t) where \code{s} and \code{t} are
#'   indexes into \code{domain}.
#' @param stopType \code{N}, for number of presentations; \code{S}, for standard deviation
#'   of the pdf; and \code{H}, for the entropy  of the pdf.
#' @param stopValue Value for number of presentations (\code{stopType=N}), standard deviation
#'   (\code{stopType=S)} or Entropy (\code{stopType=H}).
#' @param minStimulus The smallest stimuli that will be presented. Could be different from
#'   \code{domain[1]}.
#' @param maxStimulus The largest stimuli that will be presented. Could be different from
#'   \code{tail(domain,1)}.
#' @param minNotSeenLimit Will terminate if \code{minStimulus} value is not seen this many times.
#' @param maxSeenLimit Will terminate if \code{maxStimulus} value is seen this many times.
#' @param maxPresentations Maximum number of presentations regarless of \code{stopType}.
#' @param minInterStimInterval If both \code{minInterStimInterval} and \code{maxInterStimInterval}
#'   are not \code{NA}, then between each stimuli there is a random wait period drawn uniformly
#'   between \code{minInterStimInterval} and \code{maxInterStimInterval}.
#' @param maxInterStimInterval \code{minInterStimInterval}.
#' @param verbose \code{verbose=0} does nothing, \code{verbose=1} stores pdfs for returning,
#'   and \code{verbose=2} stores pdfs and also prints each presentaion.
#' @param makeStim A function that takes a dB value and numPresentations and returns an OPI datatype
#' ready for passing to opiPresent. See examples.
#' @param stimChoice A true ZEST procedure uses the \code{"mean"} of the current pdf as the stimulus,
#' but \code{"median"} and \code{"mode"} (as used in a QUEST procedure) are provided for your
#' enjoyment.
#' @param ... Extra parameters to pass to the opiPresent function
#' @details     This is an implementation of King-Smith et al.'s ZEST procedure and Watson and Pelli's
#' QUEST procedure. All presentaions are rounded to an element of the supplied domain.
#'
#' Note this function will repeatedly call \code{opiPresent} for a stimulus until \code{opiPresent}
#' returns \code{NULL} (ie no error occured).
#'
#' The \code{checkFixationOK} function is called (if present in stim made from \code{makeStim}) 
#' after each presentation, and if it returns FALSE, the pdf for that location is not changed
#' (ie the presentation is ignored), but the stim, number of presentations etc is recorded in
#' the state.
#'
#' If more than one ZEST is to be interleaved (for example, testing multiple locations), then the
#' \code{ZEST.start}, \code{ZEST.step}, \code{ZEST.stop} and \code{ZEST.final} calls can maintain
#' the state of the ZEST after each presentation, and should be used. If only a single ZEST is
#' required, then the simpler \code{ZEST} can be used, which is a wrapper for the four functions
#' that maintain state. See examples below.
#' @return
#' \subsection{Single location}{
#'   \code{ZEST} returns a list containing
#'   \itemize{
#'     \item{npres:}{ Total number of presentations used.}
#'     \item{respSeq:}{Response sequence stored as a matrix: row 1 is dB values of stimuli, row 2
#'       is 1/0 for seen/not-seen, row 3 is fixated 1/0 (always 1 if \code{checkFixationOK} not
#'       present in stim objects returned from \code{makeStim}).}
#'     \item{pdfs:}{ If \code{verbose} is bigger than 0, then this is a list of the pdfs used for each
#'       presentation, otherwise NULL.}
#'     \item{final}{ The mean/median/mode of the final pdf, depending on \code{stimChoice}, which is
#'       the determined threshold.}
#'     \item{opiResp}{A list of responses received from each successful call to \code{opiPresent}
#'       within \code{ZEST}.}
#'   }
#' }
#' \subsection{Multilple locations}{
#'   \code{ZEST.start} returns a list that can be passed to \code{ZEST.step}, \code{ZEST.stop}, and
#'   \code{ZEST.final}. It represents the state of a ZEST at a single location at a point in time
#'   and contains the following.
#'   \itemize{
#'     \item{name:}{ \code{ZEST}}
#'     \item{}{ A copy of all of the parameters supplied to ZEST.start: \code{domain},
#'       \code{likelihood}, \code{stopType}, \code{stopValue}, \code{minStimulus}, \code{maxStimulus},
#'       \code{maxSeenLimit}, \code{minNotSeenLimit}, \code{maxPresentations}, \code{makeStim},
#'       \code{stimChoice}, \code{currSeenLimit}, \code{currNotSeenLimit}, and \code{opiParams}.}
#'     \item{pdf:}{ Current pdf: vector of probabilities the same length as \code{domain}.}
#'     \item{numPresentations:}{ The number of times \code{ZEST.step} has been called on this state.}
#'     \item{stimuli:}{ A vector containing the stimuli used at each call of \code{ZEST.step}.}
#'     \item{responses:}{ A vector containing the responses received at each call of
#'       \code{ZEST.step}.}
#'     \item{responseTimes:}{ A vector containing the response times received at each call of
#'       \code{ZEST.step}.}
#'     \item{fixated:}{ A vector containing TRUE/FALSE if fixation was OK according to
#'       \code{checkFixationOK} for each call of \code{ZEST.step} (defaults to TRUE if
#'       \code{checkFixationOK} not present).}
#'     \item{opiResp}{A list of responses received from each call to \code{opiPresent} within \code{ZEST.step}.}
#'   }
#'   \code{ZEST.step} returns a list containing
#'   \itemize{
#'     \item{state:}{ The new state after presenting a stimuli and getting a response.}
#'     \item{resp:}{ The return from the \code{opiPresent} call that was made.}
#'   }
#'   \code{ZEST.stop} returns \code{TRUE} if the ZEST has reached its stopping criteria, and
#'     \code{FALSE} otherwise.
#'   \code{ZEST.final} returns an estimate of threshold based on state. If \code{state$stimChoice}
#'   is \code{mean} then the mean is returned. If \code{state$stimChoice} is \code{mode} then the
#'   mode is returned. If \code{state$stimChoice} is \code{median} then the median is returned.
#' }
#' @references
#' P.E. King-Smith, S.S. Grigsny, A.J. Vingrys, S.C. Benes, and A. Supowit. "Efficient and Unbiased
#' Modifications of the QUEST Threshold Method: Theory, Simulations, Experimental Evaluation and
#' Practical Implementation", Vision Research 34(7) 1994. Pages 885-912.
#'
#' A.B. Watson and D.G. Pelli. "QUEST: A Bayesian adaptive psychophysical method", Perception and
#' Psychophysics 33 1983. Pages 113-l20.
#'
#' A. Turpin, P.H. Artes and A.M. McKendrick "The Open Perimetry Interface: An enabling tool for
#' clinical visual psychophysics", Journal of Vision 12(11) 2012.
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}
#' @examples
#' chooseOpi("SimHenson")
#' if(!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#'
#' ##############################################
#' # This section is for single location ZESTs
#' ##############################################
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) { 
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#'
#' repp <- function(...) sapply(1:50, function(i) ZEST(makeStim=makeStim, ...))
#' a <- repp(stopType="H", stopValue=  3, verbose=0, tt=30, fpr=0.03)
#' b <- repp(stopType="S", stopValue=1.5, verbose=0, tt=30, fpr=0.03)
#' c <- repp(stopType="S", stopValue=2.0, verbose=0, tt=30, fpr=0.03)
#' d <- repp(stopType="N", stopValue= 50, verbose=0, tt=30, fpr=0.03)
#' e <- repp(prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03)
#' f <- repp(prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03)
#' g <- repp(prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03)
#' h <- repp(prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03)
#'
#' layout(matrix(1:2,1,2))
#' boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["final",])))
#' boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["npres",])))
#'
#' ##############################################
#' # This section is for multiple ZESTs
#' ##############################################
#' makeStimHelper <- function(db,n, x, y) {  # returns a function of (db,n)
#'   ff <- function(db, n) db+n
#'   body(ff) <- substitute({
#'     s <- list(x=x, y=y, level=dbTocd(db), size=0.43, color="white",
#'               duration=200, responseWindow=1500, checkFixationOK=NULL)
#'     class(s) <- "opiStaticStimulus"
#'     return(s)
#'   }, list(x=x,y=y))
#'   return(ff)
#' }
#'
#' # List of (x, y, true threshold) triples
#' locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))
#'
#' # Setup starting states for each location
#' states <- lapply(locations, function(loc) {
#'   ZEST.start(
#'     domain=-5:45,
#'     minStimulus=0,
#'     maxStimulus=40,
#'     makeStim=makeStimHelper(db,n,loc[1],loc[2]),
#'     stopType="S", stopValue= 1.5, tt=loc[3], fpr=0.03, fnr=0.01)})
#'
#' # Loop through until all states are "stop"
#' while(!all(st <- unlist(lapply(states, ZEST.stop)))) {
#'   i <- which(!st)                         # choose a random,
#'   i <- i[runif(1, min=1, max=length(i))]  # unstopped state 
#'   r <- ZEST.step(states[[i]])             # step it
#'   states[[i]] <- r$state                  # update the states
#' }
#'
#' finals <- lapply(states, ZEST.final)    # get final estimates of threshold
#' for(i in 1:length(locations)) {
#'   #cat(sprintf("Location (%+2d,%+2d) ",locations[[i]][1], locations[[i]][2]))
#'   #cat(sprintf("has threshold %4.2f\n", finals[[i]]))
#' }
#'
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
#' @export
ZEST <- function(domain=0:40, prior=rep(1/length(domain),length(domain)),
                 likelihood=sapply(domain, function(tt) 0.03 + (1-0.03-0.03)*(1-pnorm(domain, tt, 1))),
                 stopType="S",
                 stopValue=1.5,
                 minStimulus=head(domain,1),
                 maxStimulus=tail(domain,1),
                 maxSeenLimit=2,
                 minNotSeenLimit=2,
                 maxPresentations=100,
                 minInterStimInterval=NA,
                 maxInterStimInterval=NA,
                 verbose=0, makeStim,
                 stimChoice="mean",
                 ...) {
    state <- ZEST.start(domain, prior, likelihood, stopType, stopValue, 
                        minStimulus, maxStimulus, 
                        maxSeenLimit,minNotSeenLimit,maxPresentations,
                        makeStim,stimChoice, ...)
    
    pdfs <- NULL
    while(!ZEST.stop(state)) {
        r <- ZEST.step(state)
        state <- r$state
        if (verbose == 2) {
            cat(sprintf("Presentation %2d: ", state$numPresentations))
            cat(sprintf("stim= %5s repsonse=%s ", tail(state$stimuli,1), tail(state$responses,1)))
            cat(sprintf("fixation= %1.0g ", tail(state$fixated,1)))
            cat(sprintf("stdev= %8.4g H= %8.4g\n", ZEST.stdev(state), ZEST.entropy(state)))
        }
        if (verbose > 0)
            pdfs <- c(pdfs, list(state$pdf))
        
        if (!is.na(minInterStimInterval) && !is.na(maxInterStimInterval))
            Sys.sleep(runif(1, min=minInterStimInterval, max=maxInterStimInterval)/1000)
    }
    
    return(list(
        npres=tail(state$numPresentations,1),        # number of presentations
        respSeq=mapply(c, state$stimuli, state$responses, state$fixated), # reposnse sequence (list of triples)
        pdfs=pdfs,                                   # list of pdfs used (if verbose > 0)
        final=ZEST.final(state),                     # final threshold estimate
        opiResp=state$opiResp                        # list of all responses from opiPresent
    ))
}#ZEST

#' @rdname ZEST
#' @export
ZEST.start <- function(domain=0:40, prior=rep(1/length(domain),length(domain)),
            likelihood=sapply(domain, function(tt) 0.03 + (1-0.03-0.03)*(1-pnorm(domain, tt, 1))),
            stopType="S",
            stopValue=1.5,
            minStimulus=head(domain, 1),
            maxStimulus=tail(domain, 1),
            maxSeenLimit=2,
            minNotSeenLimit=2,
            maxPresentations=100,
            makeStim,
            stimChoice="mean",
            ...) {
    ##########################
    # Validate params
    ##########################
    if (!is.element(stopType, c("S", "H", "N")))
        stop("ZEST.start: stopType must be one of 'S', 'N', or 'H'")
    if (nrow(likelihood) != length(domain))
        stop(paste("ZEST.start: not enough rows in likelihood. Expect",length(domain)))
    if (ncol(likelihood) != length(domain))
        stop(paste("ZEST.start: not enough cols in likelihood. Expect",length(domain)))

    if (!is.element(minStimulus, domain))
        warning(paste("ZEST.start: you specified minStimulus=",minStimulus,"but it is not in domain."))
    if (!is.element(maxStimulus, domain))
        warning(paste("ZEST.start: you specified maxStimulus=",maxStimulus,"but it is not in domain."))

    pdf <- prior/sum(prior)

    return(list(name="ZEST",
                domain=domain, 
                pdf=pdf,
                likelihood=likelihood, 
                stopType=stopType,
                stopValue=stopValue,
                minStimulus=minStimulus,
                maxStimulus=maxStimulus,
                maxSeenLimit=maxSeenLimit,
                minNotSeenLimit=minNotSeenLimit,
                maxPresentations=maxPresentations,
                makeStim=makeStim,
                stimChoice=stimChoice,
                currSeenLimit=0,                    # number of times maxStimulus seen
                currNotSeenLimit=0,                 # number of times minStimulus not seen
                numPresentations=0,                 # number of presentations so far
                stimuli=NULL,                       # vector of stims shown
                responses=NULL,                     # vector of responses (1 seen, 0 not)
                responseTimes=NULL,                 # vector of response times
                fixated=NULL,                       # vector of true/false for fixated one per stimuli
                opiResp=NULL,                       # list of opiPresent return values
                opiParams=list(...)                 # the extra params
            ))
}# ZEST.start

#' @rdname ZEST
#' @param state Current state of the ZEST returned by \code{ZEST.start} and \code{ZEST.step}.
#' @param nextStim A valid object for \code{opiPresent} to use as its \code{nextStim}.
#' @export
ZEST.step <- function(state, nextStim=NULL) {

    if (state$stimChoice == "mean") {
        stimIndex <- which.min(abs(state$domain - sum(state$pdf * state$domain)))
    } else if (state$stimChoice == "mode") {
        stimIndex <- which.max(state$pdf)
    } else if (state$stimChoice == "median") {
        stimIndex <- which.min(abs(cumsum(state$pdf) - 0.5))
    } else {
        stop(paste("ZEST.step: stimChoice = ",state$stimChoice," not implemented."))
    }
    stim <- state$domain[stimIndex]
    stim <- max(stim, state$minStimulus)   # check not outside [minStimulus,maxStimulus]
    stim <- min(stim, state$maxStimulus)

    params <- c(list(stim=state$makeStim(stim, state$numPresentations), nextStim=nextStim), state$opiParams)
    opiResp <- do.call(opiPresent, params)
    while (!is.null(opiResp$err))
        opiResp <- do.call(opiPresent, params)

    fixation_is_good <- TRUE
    if (!is.null(params$stim$checkFixationOK)) {
        fixation_is_good <- params$stim$checkFixationOK(opiResp)
    }

    state$stimuli          <- c(state$stimuli, stim)
    state$responses        <- c(state$responses, opiResp$seen)
    state$responseTimes    <- c(state$responseTimes, opiResp$time)
    state$numPresentations <- state$numPresentations + 1
    state$fixated          <- c(state$fixated, fixation_is_good)
    state$opiResp          <- c(state$opiResp, list(opiResp))

    if (fixation_is_good) {  # update the pdf
        if(opiResp$seen) { 
            if (stim == state$maxStimulus) state$currSeenLimit <- state$currSeenLimit + 1
            state$pdf <- state$pdf * state$likelihood[stimIndex, ]
        } else {
            if (stim == state$minStimulus) state$currNotSeenLimit <- state$currNotSeenLimit + 1
            state$pdf <- state$pdf * (1 - state$likelihood[stimIndex, ])
        }
        state$pdf <- state$pdf/sum(state$pdf)
    } else {
        warning("ZEST.step: fixation lost during presentation, pdf not updated")
    }

    return(list(state=state, resp=opiResp))
}#ZEST.step()

#' @rdname ZEST
#' @export
ZEST.stop <- function(state) {
    keepGoing <- (
        (state$numPresentations < state$maxPresentations) &&
        (state$currNotSeenLimit < state$minNotSeenLimit) &&
        (state$currSeenLimit    < state$maxSeenLimit) &&
        (
           ((state$stopType == "S") && (ZEST.stdev(state) > state$stopValue))
        || ((state$stopType == "H") && (ZEST.entropy(state) > state$stopValue))
        || ((state$stopType == "N") && (state$numPresentations < state$stopValue))
        )
    )
    return (!keepGoing)
}#ZEST.stop

#' @rdname ZEST
#' @export
ZEST.final <- function(state) {
    if (state$stimChoice == "mean") {
        final <- sum(state$pdf*state$domain)
    } else if (state$stimChoice == "mode") {
        final <- state$domain[which.max(state$pdf)]
    } else if (state$stimChoice == "median") {
        final <- state$domain[which.min(abs(cumsum(state$pdf) - 0.5))]
    } 

    return(final)
}#ZEST.final

############################################################
# Tests
############################################################
#require(OPI)
#chooseOpi("SimHenson")
##chooseOpi("SimYes")
#opiInitialize("C",6)
#
#makeStim <- function(db, n) { 
#         s <- list(x=9, y=9, level=dbTocd(db,10000/pi), size=0.43, 
#                  color="white",
#                  duration=200, responseWindow=1500, 
#                   checkFixationOK=NULL)
#         class(s) <- "opiStaticStimulus"
#     
#         return(s)
#     }
#makeNextStim <- function(x,y) { 
#         s <- list(x=9, y=9, level=dbTocd(db,10000/pi), size=0.43, color="white",
#                  duration=200, responseWindow=1500, checkFixationOK=NULL)
#         class(s) <- "opiStaticStimulus"
#     
#         return(s)
#     }
#
#state <- ZEST.start(domain=-5:45, maxStimulus=40, minStimulus=0, makeStim=makeStim, stopType="S", stopValue= 1.5, tt=0, fpr=0.30)
#while(!ZEST.stop(state)) {
#    r <- ZEST.step(state)
#    cat(sprintf("%2d %s\n",tail(r$state$stimuli,1), r$resp$seen))
#    state <- r$state
#}
#print(ZEST.final(state))
#
#print("########################################################################")
#
#makeStimHelper <- function(db,n, x, y) {
#    ff <- function(db, n) db+n
#
#    body(ff) <- substitute(
#        {s <- list(x=x, y=y, level=dbTocd(db,10000/pi), size=0.43, color="white",
#                  duration=200, responseWindow=1500, checkFixationOK=NULL)
#         class(s) <- "opiStaticStimulus"
#         return(s)
#        }
#        , list(x=x,y=y)) 
#    return(ff)
#} 
#
#    # list of (x, y, true threshold) triples
#locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))
#
#    # setup starting states for each location
#states <- lapply(locations, function(loc) {
#    ZEST.start(domain=-5:45,
#        makeStim=makeStimHelper(db,n,loc[1],loc[2]),
#        maxStimulus=40, minStimulus=0,         
#        stopType="S", stopValue= 1.5, tt=loc[3], fpr=0.03, fn=0.01)
#})
#
#    # loop through until all states are "stop"
#while(!all(st <- unlist(lapply(states, ZEST.stop)))) {
#    i <- sample(which(!st), 1)  # choose a random, unstopped state
#    r <- ZEST.step(states[[i]]) # step it
#    states[[i]] <- r$state      # update the states
#}
#
#finals <- lapply(states, ZEST.final)    # get final estimates of threshold
#for(i in 1:length(locations))
#    cat(sprintf("Location (%+2d,%+2d) has threshold %4.2f\n",locations[[i]][1], locations[[i]][2], finals[[i]]))
#    
#print("########################################################################")
#
#a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="H", stopValue=  3, verbose=0, tt=20, fpr=0.03))
#b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="S", stopValue=1.5, verbose=0, tt=20, fpr=0.03))
#c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="S", stopValue=2.0, verbose=0, tt=20, fpr=0.03))
#d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stopType="N", stopValue= 50, verbose=0, tt=20, fpr=0.03))
#
#a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stimChoice="mean", tt=20, fpr=0.03))
#b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stimChoice="mode", tt=20, fpr=0.03))
#c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stimChoice="median", tt=20, fpr=0.03))
#d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, stimChoice="mean", tt=20, fpr=0.03))
#
#layout(matrix(1:2,1,2))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["final",])))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["npres",])))
#
#a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03))
#b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03))
#c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03))
#d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03))
#layout(matrix(1:2,1,2))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["final",])))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["npres",])))
#
#repp <- function(...) sapply(1:100, function(i) ZEST(makeStim=makeStim, ...))
#a <- repp(stopType="H", stopValue=  3, verbose=0, tt=30, fpr=0.03)
#b <- repp(stopType="S", stopValue=1.5, verbose=0, tt=30, fpr=0.03)
#c <- repp(stopType="S", stopValue=2.0, verbose=0, tt=30, fpr=0.03)
#d <- repp(stopType="N", stopValue= 50, verbose=0, tt=30, fpr=0.03)
#e <- repp(prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03)
#f <- repp(prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03)
#g <- repp(prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03)
#h <- repp(prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03)
#
#layout(matrix(1:2,1,2))
#boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["final",])))
#boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["npres",])))
#
#a <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=00, fpr=0.03))
#b <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=10, fpr=0.03))
#c <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=20, fpr=0.03))
#d <- sapply(1:100, function(i) ZEST(makeStim=makeStim, prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03))
#layout(matrix(1:2,1,2))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["final",])))
#boxplot(lapply(list(a,b,c,d), function(x) unlist(x["npres",])))
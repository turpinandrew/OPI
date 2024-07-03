# # Psi algorithm of Kontsevich and Tyler that maintains state in a list.
# Includes
#     KTPsi          # just for a single location
#     KTPsi.start    # initialise list state
#     KTPsi.step     # take state, present stim, update and return state
#     KTPsi.stop     # boolean - true if state is finished
#     KTPsi.final    # return final estimate from state
#
# Author: Andrew Turpin
# Date: August 2023
#
# Copyright [2023] [Andrew Turpin]
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


#' @rdname KTPsi
#' @title An implementation of Kontsevich and Tyler \eqn{\Psi} algorithm.
#'
#' @description
#' An implementation of Kontsevich and Tyler (Vis Res 39 (1999) pages 2729--2737
#' default parameterised for Standard Automated Perimetry.
#' based on
#'     A. Turpin, D. Jankovic and A.M. McKendrick,
#'     "Identifying Steep Psychometric Function Slope Quickly in Clinical Applications",
#'     Vision Research, 50(23). November 2010. Pages 2476-2485
#'
#' @param domains A list of 4 vectors:
#'  * \code{slopes} The valid slopes in the domain of psychometric functions.
#'  * \code{thresholds} The valid thresholds in the domain of psychometric functions.
#'  * \code{fps} The valid upper asymptotes (false positives) in the domain of psychometric functions.
#'  * \code{fns} The valid lower asymptotes (false negatives) in the domain of psychometric functions.
#'
#' @param priors A list of 4 vectors:
#'  * \code{slopes} The prior probability vector for \code{domains$slopes}.
#'  * \code{thresholds} The prior probability vector for \code{domains$thresholds}.
#'  * \code{fps} The prior probability vector for \code{domains$fps}.
#'  * \code{fns} The prior probability vector for \code{domains$fns}.
#'
#' Each prior should the same length as its `domains` counterpart and sum to 1.
#'
#' @param stimValues Vector of allowable stimulus values.
#' @param stopType \code{N}, for number of presentations and \code{H}, for the entropy  of the pdf.
#' @param stopValue Value for number of presentations (\code{stopType=N}), or Entropy (\code{stopType=H}).
#' @param maxPresentations Maximum number of presentations regardless of \code{stopType}.
#' @param minInterStimInterval If both \code{minInterStimInterval} and \code{maxInterStimInterval}
#'   are not \code{NA}, then between each stimuli there is a random wait period drawn uniformly
#'   between \code{minInterStimInterval} and \code{maxInterStimInterval}.
#' @param maxInterStimInterval \code{minInterStimInterval}.
#' @param verbose \code{verbose=0} does nothing, \code{verbose=1} stores pdfs for returning,
#'   and \code{verbose=2} stores pdfs and also prints each presentation.
#' @param makeStim A function that takes a stimulus value and numPresentations and returns an OPI datatype
#' ready for passing to opiPresent. See examples.
#' @param ... Extra parameters to pass to the opiPresent function
#'
#' @details
#' The assumed psychometric function is the cumulative Gaussian:
#' \deqn{\mbox{fp}+(1-\mbox{fp}-\mbox{fn})(1-\mbox{pnorm}(x, \mbox{threshold}, \mbox{slope})}
#' hence `domain$slopes` are standard deviations and `domain$thresholds` are the mean.
#'
#' While it is assumed that `domains$thresholds` and `stimValues` are in dB, this need not be the case.
#' As long as the `makeStim` function converts `stimValues` into cd/\eqn{\mbox{m}^2}{m^2}
#' for the `opiPresent` function, then any units should work.
#'
#' The \code{checkFixationOK} function is called (if present in stim made from \code{makeStim})
#' after each presentation, and if it returns FALSE, the pdf for that state is not changed
#' (ie the presentation is ignored), but the stim, number of presentations etc is recorded in
#' the state.
#'
#' If more than one KTPsi is to be interleaved (for example, testing multiple locations), then thePsi
#' \code{KTPsi.start}, \code{KTPsi.step}, \code{KTPsi.stop} and \code{KTPsi.final} calls can maintain
#' the state of the KTPsi after each presentation, and should be used. If only a single KTPsi is
#' required, then the simpler \code{KTPsi} function can be used, which is a wrapper for the four functions
#' that maintain state. See examples below.
#'
#' @return
#' ## Single location
#' \code{KTPsi} returns a list containing
#'   * \code{npres} Total number of presentations used.
#'   * \code{respSeq} Response sequence stored as a matrix: row 1 is dB values of stimuli, row 2 is 1/0 for seen/not-seen, row 3 is fixated 1/0 (always 1 if \code{checkFixationOK} not present in stim objects returned from \code{makeStim}).
#'   * \code{pdfs} If \code{verbose} is bigger than 0, then this is a list of the pdfs used for each presentation, otherwise NULL.
#'   * \code{final} The mean/median/mode of the final pdf, depending on \code{stimChoice}, which is the determined threshold.
#'   * \code{opiResp} A list of responses received from each successful call to \code{opiPresent} within \code{KTPsi}.

#' ## Multilple locations
#' \code{KTPsi.start} returns a list that can be passed to \code{KTPsi.step}, \code{KTPsi.stop}, and \code{KTPsi.final}. It represents the state of a KTPsi at a single location at a point in time and contains the following.
#'   * \code{name} \code{KTPsi}
#'   * A copy of all of the parameters supplied to KTPsi.start: \code{domains}, \code{priors}, \code{stimValues}, \code{stopType}, \code{stopValue}, \code{maxPresentations}, \code{makeStim} and \code{opiParams}.
#'   * \code{psi} A matrix where \code{psi[domain_index, stim]} is the probability of seeing \code{stim} assuming the psychometric function for the domain index \code{domain_index}.
#'   * \code{labels} A text representation of \code{psi[domain_index, ]}, or the the psychometric function for the domain index \code{domain_index}.
#'   * \code{pdf} Current pdf: vector of probabilities the same length as product of lengths of \code{domain} elements.
#'   * \code{numPresentations} The number of times \code{KTPsi.step} has been called on this state.
#'   * \code{stimuli} A vector containing the stimuli used at each call of \code{KTPsi.step}.
#'   * \code{responses} A vector containing the responses received at each call of \code{KTPsi.step}.
#'   * \code{responseTimes} A vector containing the response times received at each call of \code{KTPsi.step}.
#'   * \code{fixated} A vector containing TRUE/FALSE if fixation was OK according to \code{checkFixationOK} for each call of \code{KTPsi.step} (defaults to TRUE if \code{checkFixationOK} not present).
#'   * \code{opiResp} A list of responses received from each call to \code{opiPresent} within \code{KTPsi.step}.
#'
#' \code{KTPsi.step} returns a list containing
#'   *  \code{stat:} The new state after presenting a stimuli and getting a response.
#'   *  \code{resp} The return from the \code{opiPresent} call that was made.
#'
#' \code{KTPsi.stop} returns \code{TRUE} if the KTPsi has reached its stopping criteria, and \code{FALSE} otherwise.
#' 
#' \code{KTPsi.final} returns an estimate of threshold based on state based on its parameter.
#'
#' @references
#' Kontsevich and Tyler. Vision Research 39 (1999) pages 2729--2737.
#'
#' A. Turpin, D. Jankovic and A.M. McKendrick,
#' "Identifying Steep Psychometric Function Slope Quickly in Clinical Applications",
#' Vision Research, 50(23). November 2010. Pages 2476-2485
#'
#' A. Turpin, P.H. Artes and A.M. McKendrick "The Open Perimetry Interface: An enabling tool for
#' clinical visual psychophysics", Journal of Vision 12(11) 2012.
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}
#'
#' @examples
#' chooseOpi("SimGaussian")
#' if(!is.null(opiInitialize(sd = 2)$err))
#'   stop("opiInitialize failed")
#'
#'      # This section is for single location KTPsi
#'      # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#'
#' KTPsi(makeStim = makeStim, stopType="H", stopValue=  3, tt=30, fpr=0.03)
#' KTPsi(makeStim = makeStim, stopType="N", stopValue= 27, verbose = 0, tt=30, fpr=0.03)
#'
#'      # For multiple locations...
#' \dontrun{
#' states <- lapply(1:10, function(loc) KTPsi.start(makeStim = makeStim))
#' unfinished <- 1:10
#' while (length(unfinished) > 0) {
#'      loc <- unfinished[[1]]
#'      states[[loc]] <- KTPsi.step(states[[loc]])$state
#'      if (KTPsi.stop(states[[loc]]))
#'          unfinished <- unfinished[-1]
#' }
#' }
#'
#' @export
KTPsi <- function(
    domains = list(
        slopes = 1:5,
        thresholds = 20:40,
        fps = c(0, 0.025, 0.05, 0.1, 0.2),
        fns = c(0, 0.025, 0.05, 0.1, 0.2)
    ),
    priors = list(
        slopes = rep(1, length(domains$slopes)) / length(domains$slopes),
        thresholds = rep(1, length(domains$thresholds)) / length(domains$thresholds),
        fps = rep(1, length(domains$fps)) / length(domains$fps),
        fns = rep(1, length(domains$fns)) / length(domains$fns)
    ),
    stimValues = 17:40,
    stopType = "N",
    stopValue = 140,
    maxPresentations = 200, # in case stopType == H
    minInterStimInterval = NA,
    maxInterStimInterval = NA,
    verbose = 0,
    makeStim,
    ...) {

    state <- KTPsi.start(domains = domains, priors = priors, stimValues = stimValues, stopType = stopType,
                      stopValue = stopValue, maxPresentations = maxPresentations, makeStim = makeStim, ...)
    pdfs <- NULL
    while (!KTPsi.stop(state)) {
        rs <- KTPsi.step(state)
        state <- rs$state

        if (verbose == 2) {
            cat(sprintf("Presentation %2d: ", state$numPresentations))
            cat(sprintf("stim= %5s response=%6s ", utils::tail(state$stimuli, 1), rs$resp$seen))
            cat(sprintf("fixation= %1.0g ", utils::tail(state$fixated, 1)))
            cat(sprintf("H= %8.4g\n", state$H))
        }
        if (verbose > 0)
            pdfs <- c(pdfs, list(state$pdf))

        if (!is.na(minInterStimInterval) && !is.na(maxInterStimInterval))
            Sys.sleep(stats::runif(1, min = minInterStimInterval, max = maxInterStimInterval)/1000)
    }
    return(list(
        final = KTPsi.final(state),
        npres = state$numPresentation,
        respSeq = mapply(c, state$stimuli, state$responses, state$fixated), # response sequence (list of triples)
        pdf = pdfs,
        opiResp = state$opiResp
    ))
}


#' @rdname KTPsi
#' @export
KTPsi.start <- function(
    domains = list(
        slopes = 1:5,
        thresholds = 20:40,
        fps = c(0, 0.025, 0.05, 0.1, 0.2),
        fns = c(0, 0.025, 0.05, 0.1, 0.2)
    ),
    priors = list(
        slopes = rep(1, length(domains$slopes)) / length(domains$slopes),
        thresholds = rep(1, length(domains$thresholds)) / length(domains$thresholds),
        fps = rep(1, length(domains$fps)) / length(domains$fps),
        fns = rep(1, length(domains$fns)) / length(domains$fns)
    ),
    stimValues = 17:40,
    stopType = "N",
    stopValue = 140,
    maxPresentations = 200, # in case stopType == H
    minInterStimInterval = NA,
    maxInterStimInterval = NA,
    verbose = 0,
    makeStim,
    ...) {
        # Validate params
    if (!is.element(stopType, c("N", "H")))
        stop("KTPsi.start: stopType must be one of 'N', or 'H'")

    for (n in c("slopes", "thresholds", "fps", "fns")) {
        if (!n %in% names(domains))
            stop(sprintf("KTPsi domains list parameter does not contain %s", n))
        if (!n %in% names(priors))
            stop(sprintf("KTPsi priors list parameter does not contain %s", n))
        if (length(domains[[n]]) != length(priors[[n]]))
            stop(sprintf("KTPsi parameter priors$%s (%s) is not the same length as domains$%s (%s)",
                n, length(priors[[n]]), n, length(domains[[n]])))
    }

        # Build single domain and associated tables
    domain <- seq_len(prod(unlist(lapply(domains, length))))
    psi <- matrix(NA, nrow = length(domain), ncol = length(stimValues))
    labels <- vector("list", length(domain))
    pdf <- rep(NA, length(domain))
    pindex <- 1
    for (i_s in seq_along(domains$slopes))
    for (i_t in seq_along(domains$thresholds))
    for (i_fp in seq_along(domains$fps))
    for (i_fn in seq_along(domains$fns)) {
        s <- domains$slopes[[i_s]]
        t <- domains$thresholds[[i_t]]
        fp <- domains$fps[[i_fp]]
        fn <- domains$fns[[i_fn]]
        psi[pindex, ] <- sapply(stimValues, function(stim) fp + (1 - fp - fn) * (1 - stats::pnorm(stim, t, s)))
        labels[[pindex]] <- c(s, t, fp, fn)
        pdf[[pindex]] <- priors$slopes[[i_s]] * priors$thresholds[[i_t]] * priors$fps[[i_fp]] * priors$fns[[i_fn]]
        pindex <- pindex + 1
    }

    return(list(name = "KTPsi",
                domains = domains,
                priors = priors,
                stimValues = stimValues,
                domain = domain,
                psi = psi,
                labels = labels,
                pdf = pdf,
                stopType = stopType,
                stopValue = stopValue,
                maxPresentations = maxPresentations,
                makeStim = makeStim,
                numPresentations = 0,                 # number of presentations so far
                stimuli = NULL,                       # vector of stims shown
                responses = NULL,                     # vector of responses (1 seen, 0 not)
                responseTimes = NULL,                 # vector of response times
                fixated = NULL,                       # vector of true/false for fixated one per stimuli
                opiResp = NULL,                       # list of opiPresent return values
                opiParams = list(...),                 # the extra params
                H = sum(ifelse(pdf == 0, 0, -pdf * log2(pdf))) # Entropy of current pdf (set after one pres)
            ))
}# KTPsi.start


#' @rdname KTPsi
#' @param state Current state of the KTPsi as returned by (eg) \code{KTPsi.start}.
#' @param nextStim The next stimulus to present in a suitable format for passing to \code{\link{opiPresent}}
#' @param fixedStimValue Currently ignored.
#' @export
KTPsi.step <- function(state, nextStim = NULL, fixedStimValue = NA) {
        # calculate prob of a "yes" to any x \in 1:length(state$stimValues)
    prob_yes <- sapply(seq_along(state$stimValues), function(i_stim) sum(state$pdf * state$psi[, i_stim]))

        # calculate probPF if yes or no for each possible stim value, x
    post_yes <- lapply(seq_along(state$stimValues), function(i_stim) {
            p <- prob_yes[[i_stim]]
            if (p == 0)
                rep(0, length(state$domain))
            else {
                state$pdf * state$psi[, i_stim] / p
            }
    })
    post_no <- lapply(seq_along(state$stimValues), function(i_stim) {
            p <- 1 - prob_yes[[i_stim]]
            if (p == 0)
                rep(0, length(state$domain))
            else {
                state$pdf * (1 - state$psi[, i_stim]) / p
            }
    })

    entropy_yes <- unlist(lapply(post_yes, function(pdf) sum(ifelse(pdf == 0, 0, -pdf * log2(pdf)))))
    entropy_no <- unlist(lapply(post_no, function(pdf) sum(ifelse(pdf == 0, 0, -pdf * log2(pdf)))))

                # calculate expected entropy for each possible stim (x)
    expectedEntropy <- entropy_yes * unlist(prob_yes) + entropy_no * (1 - unlist(prob_yes))

    stim_index <- which.min(expectedEntropy)
    stim <- state$stimValues[[stim_index]]

                # present x and get response
    params <- c(list(stim = state$makeStim(stim, state$numPresentations), nextStim = nextStim), state$opiParams)
    opiResp <- do.call(opiPresent, params)
    if (!is.null(opiResp$err))
        return(list(state = state, resp = opiResp))

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

    if (fixation_is_good) {
                # swap probPF with either the yes or no already calculated
                # better re-scale too else we will run out of precision
                # after a couple of hundered trials
        if (opiResp$seen)
            state$pdf <- post_yes[[stim_index]] / sum(post_yes[[stim_index]])
        else
            state$pdf <- post_no[[stim_index]] / sum(post_no[[stim_index]])

        state$H <- sum(ifelse(state$pdf == 0, 0, -state$pdf * log2(state$pdf)))
    } else {
        warning("KTPsi.step: fixation lost during presentation, pdf not updated")
    }

    return(list(state = state, resp = opiResp))
}

#' @rdname KTPsi
#' @param state Current state of the KTPsi as returned by (eg) \code{KTPsi.start}.
#' @param method Either \code{"expectation"} or \code{"MAP"}.
#' @export
KTPsi.final <- function(state, method = "expectation") {
    if (method == "expectation") {
        res <- round(sum(state$pdf * state$domain))
    } else if (method == "MAP") {
        res <- which.max(state$pdf)
    } else {
       warning("Unknown 'method' parameter in KTPsi.final().")
       return(list(res = NA, desc = NA))
    }
    return(list(res = res, desc = state$labels[[res]]))
}

#' @rdname KTPsi
#' @param state Current state of the KTPsi as returned by (eg) \code{KTPsi.start}.
#' @return TRUE if the `state` has reached its stopping criteria, and FALSE otherwise.
#' @export
KTPsi.stop <- function(state) {
    (state$stopType == "N" && state$numPresentations >= state$stopValue) ||
    (state$stopType == "H" && state$H <= state$stopValue)
}


###########
# Tests
###require(OPI)
###chooseOpi("SimGaussian")
#### This section is for single location KTPsi
#### Stimulus is Size III white-on-white as in the HFA
###makeStim <- function(db, n) {
###  s <- list(x = 9, y = 9, level = dbTocd(db), size = 0.43, color = "white",
###            duration = 200, responseWindow = 1500, checkFixationOK = NULL)
###  class(s) <- "opiStaticStimulus"
###  return(s)
###}
###
###repp <- function(...) sapply(1:200, function(i) KTPsi(makeStim = makeStim, ...))
###
###res <- list()
###for (true_s in 1:9) {
###    if (!is.null(opiInitialize(sd = true_s)$err)) stop("opiInitialize failed")
###
###    #pdf("kt_debug.pdf")
###    #KTPsi(makeStim = makeStim, verbose = 2, stopValue = 27, tt = 30, fnr = 0.13)
###    #dev.off()
###
###    a <- repp(stopType = "N", stopValue = 140, tt = 30, fpr = 0.03)
###    b <- repp(stopType = "N", stopValue =  27, tt = 30, fpr = 0.03)
###
###    ma <- matrix(unlist(lapply(a[1, ], "[", "desc")), ncol = 4, byrow = TRUE)
###    mb <- matrix(unlist(lapply(b[1, ], "[", "desc")), ncol = 4, byrow = TRUE)
###
###    res <- c(res, list((list(ma, mb))))
###}
###boxplot(c(
###    lapply(res, function(m) m[[1]][, 1]),
###    lapply(res, function(m) m[[2]][, 1])),
###    names = c(1:9, 1:9),
###    xlab = "True Slope",
###    ylab = "Slope", las = 1)
###points(1:9, 1:9, pch = 19, col = "red")
###points(9+1:9, 1:9, pch = 19, col = "red")
###abline(v = 9.5)
###text(4.5, 1, "140 presentations")
###text(9+4.5, 1, "27 presentations")

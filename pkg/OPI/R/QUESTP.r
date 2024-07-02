#
# QUEST+ algorithm that maintains state in a list - good for interleaved.
# Includes
#     QUESTP            # creates and runs a single QUEST+ element
#     QUESTP.Prior      # sets up priors for parameters in QUESTP
#     QUESTP.Likelihood # calculates likelihoods for QUEST+ element
#     QUESTP.start      # initialise list state
#     QUESTP.step       # take state, present stim, update and return state
#     QUESTP.stop       # boolean - true if state is finished
#     QUESTP.final      # return final estimate from state
#     QUESTP.stdev      # return standard deviation of parameters
#     QUESTP.entropy    # return final estimate from state
#
# Author: Giovanni Montesano    (giovmontesano@gmail.com)
# Date: August 2022
#
# Copyright 2022 Giovanni Montesano
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

#Function for Matlab style vector combination
combvec <- function(ListM){
    if (length(ListM) > 1){
        ListD <- list()
        for (ll in 1:length(ListM)){
            ListM[[ll]] <- matrix(ListM[[ll]], nrow = 1)
            ListD[[ll]] <- 1:ncol(ListM[[ll]])
        }
        CombInd <- expand.grid(ListD)
        Out <- NULL
        for (ll in 1:length(ListM)){
            Out <- rbind(Out, ListM[[ll]][,CombInd[,ll]])
        }
    }else{
        Out <- matrix(ListM[[1]], nrow = 1)
    }
    return(Out)
}

################################################################################
#' @rdname QUESTP
#' @title QUEST+
#' @description An implementation of the Bayesian test procedure QUEST+ by AB Watson.
#' This is mostly a translation of the MATLAB implementation by P Jones (see References).
#' Its use is similar to ZEST. The objective is to estimate parameters of a function
#' that defines the probability of responding stimuli. The steps are optimized based on entropy rather than
#' the mean or the mode of the current pdfs.
#' @param Fun Function to be evaluated, of the form \code{pseen = function(stim, param){...}}. Outputs a probability of seen.
#' @param stimDomain Domain of values for the stimulus. Can be multi-dimensional (list, one element per dimension)
#' @param paramDomain Domain of values for pdfs of the parameters in Fun. Can be multi-parametric (list, one element per parameter).
#' @param priors Starting probability distributions for the parameter domains (list, one element per parameter)
#' @param stopType \code{N}, for number of presentations; \code{S}, for standard deviation
#'   of the pdf; and \code{H}, for the entropy  of the pdf (default).
#' @param stopValue Value for number of presentations (\code{stopType=N}), standard deviation
#'   (\code{stopType=S)} or Entropy (\code{stopType=H}).
#' @param minNotSeenLimit Will terminate if \code{minStimulus} value is not seen this many times.
#' @param maxSeenLimit Will terminate if \code{maxStimulus} value is seen this many times.
#' @param minPresentations Minimum number of presentations
#' @param maxPresentations Maximum number of presentations regarless of \code{stopType}.
#' @param minInterStimInterval If both \code{minInterStimInterval} and \code{maxInterStimInterval}
#'   are not \code{NA}, then between each stimuli there is a random wait period drawn uniformly
#'   between \code{minInterStimInterval} and \code{maxInterStimInterval}.
#' @param maxInterStimInterval \code{minInterStimInterval}.
#' @param verbose \code{verbose=0} does nothing, \code{verbose=1} stores pdfs for returning,
#'   and \code{verbose=2} stores pdfs and also prints each presentaion.
#' @param makeStim A function that takes a dB value and numPresentations and returns an OPI datatype
#' ready for passing to opiPresent. See examples.
#' @param likelihoods Pre-computed likelihoods if available (for QUESTP.start)
#' @param Choice How to compute final values in QUESTP.final ("mean","mode","median")
#' @param WhichP Which parameter (numeric index) to monitor when calling QUESTP.stdev directly
#' (returns max(stdev) if unspecified)
#' @param ... Extra parameters to pass to the opiPresent function
#' @details
#' An implementation of the Bayesian test procedure QUEST+ by AB Watson.
#' This is mostly a translation of the MATLAB implementation by P Jones (see References).
#' Its use is similar to ZEST. The objective is to estimate parameters of a function
#' that defines the probability of responding to stimuli. The steps are optimized based on entropy rather than
#' the mean or the mode of the current pdfs.
#'
#' The stimulus, parameter and response domain are separate and can be multidimensional.
#' Each parameter has its own pdf. For evaluation, the pdfs are chained as a long vector
#' (so no co-variances are considered). More complex functions will require larger combined pdfs
#' and longer computation time for likelihoods and updating at each step. In these cases, it is recommended
#' to pre-calculate the likelihoods using QUESTP.Likelihood and store them.
#'
#' The function to be fitted needs to output a probability of seen (i.e. \code{pseen = function(stim, param){...}})
#' and must take \code{stim} and \code{param} as inputs. \code{stim} is a vector with length = number of stimulus dimensions
#' (in simple one-dimensional cases, the intensity in dB). \code{param} is a vector with length = number
#' of parameters to be fitted in Fun.
#'
#' For example, QUEST+ can fit a Gaussian psychometric function with stimDomain = \code{{0, 1,..., 39, 40}} dB
#' and paramDomain = \code{({0, 1,..., 39, 40}; {0.5, 1,..., 5.5, 6})} dB for the mean and
#' standard deviation respectively. A standard ZEST procedure can be replicated by setting
#' stimDomain = \code{{0, 1,..., 39, 40}} dB and paramDomain = \code{({0, 1,..., 39, 40}; {1})} dB, i.e. by setting the
#' stimDomain = paramDomain for the mean and by having a static standard deviation = 1 dB. Note however that
#' the stimulus selection is always based on entropy and not on the mean/mode of the current pdf. See examples below
#'
#' Note this function will repeatedly call \code{opiPresent} for a stimulus until \code{opiPresent}
#' returns \code{NULL} (ie no error occurred).
#'
#' The \code{checkFixationOK} function is called (if present in stim made from \code{makeStim})
#' after each presentation, and if it returns FALSE, the pdf for that location is not changed
#' (ie the presentation is ignored), but the stim, number of presentations etc is recorded in
#' the state.
#'
#' If more than one QUESTP is to be interleaved (for example, testing multiple locations), then the
#' \code{QUESTP.start}, \code{QUESTP.step}, \code{QUESTP.stop} and \code{QUESTP.final} calls can maintain
#' the state of the QUESTP after each presentation, and should be used. If only a single QUESTP is
#' required, then the simpler \code{QUESTP} can be used, which is a wrapper for the four functions
#' that maintain state. See examples below.
#' @return
#' \subsection{Single location}{
#'   \code{QUESTP} returns a list containing
#'   \itemize{
#'     \item{npres:}{ Total number of presentations used.}
#'     \item{respSeq:}{ Response sequence stored as a data frame: column 1 is a string identified of a (potentially)
#'       multidimensional stimulus values of stimuli (dimensions chained into a string), column 2
#'       is 1/0 for seen/not-seen, column 3 is fixated 1/0 (always 1 if \code{checkFixationOK} not
#'       present in stim objects returned from \code{makeStim}). All additional columns report each stimulus
#'       dimension, one for each row}
#'     \item{pdfs:}{ If \code{verbose} is bigger than 0, then this is a list of the pdfs used for each
#'       presentation, otherwise NULL.}
#'     \item{final}{ The mean (default, strongly suggested)/median/mode of the parameters' pdf, depending on \code{Choice}.}
#'     \item{opiResp}{A list of responses received from each successful call to \code{opiPresent}
#'       within \code{QUESP}.}
#'   }
#' }
#' \subsection{Multilple locations}{
#'   \code{QUESTP.start} returns a list that can be passed to \code{QUESTP.step}, \code{QUESTP.stop}, and
#'   \code{QUESTP.final}. It represents the state of a QUESTP at a single location at a point in time
#'   and contains the following.
#'   \itemize{
#'     \item{name:}{ \code{QUESTP}}
#'     \item{}{ A copy of all of the parameters supplied to QUESTP.start: \code{stimDomain},
#'       \code{paramDomain}, \code{likelihoods}, \code{priors}, \code{stopType}, \code{stopValue},
#'       \code{maxSeenLimit}, \code{minNotSeenLimit}, \code{minPresentations}, \code{maxPresentations},
#'       \code{makeStim}, and \code{opiParams}.}
#'     \item{pdf:}{ Current pdf: vector of probabilities, collating all parameter domains.}
#'     \item{priorsP:}{ List of starting pdfs, one for each parameter.}
#'     \item{numPresentations:}{ The number of times \code{QUESTP.step} has been called on this state.}
#'     \item{stimuli:}{ A vector containing the stimuli used at each call of \code{QUESTP.step}.}
#'     \item{responses:}{ A vector containing the responses received at each call of
#'       \code{QUESTP.step}.}
#'     \item{responseTimes:}{ A vector containing the response times received at each call of
#'       \code{QUESTP.step}.}
#'     \item{fixated:}{ A vector containing TRUE/FALSE if fixation was OK according to
#'       \code{checkFixationOK} for each call of \code{QUESTP.step} (defaults to TRUE if
#'       \code{checkFixationOK} not present).}
#'     \item{opiResp}{A list of responses received from each call to \code{opiPresent} within \code{QUESTP.step}.}
#'   }
#'   \code{QUESTP.step} returns a list containing
#'   \itemize{
#'     \item{state:}{ The new state after presenting a stimuli and getting a response.}
#'     \item{resp:}{ The return from the \code{opiPresent} call that was made.}
#'   }
#'   \code{QUESTP.stop} returns \code{TRUE} if the QUESTP has reached its stopping criteria, and
#'     \code{FALSE} otherwise.
#'   \code{QUESTP.final} returns an estimate of parameters based on state. If \code{state$Choice}
#'   is \code{mean} then the mean is returned (the only one that really makes sense for QUEST+).
#'   If \code{state$Choice} is \code{mode} then the
#'   mode is returned. If \code{state$Choice} is \code{median} then the median is returned.
#' }
#' @references
#' Andrew B. Watson; QUEST+: A general multidimensional Bayesian adaptive psychometric method.
#' Journal of Vision 2017;17(3):10. doi: https://doi.org/10.1167/17.3.10.
#'
#' Jones, P. R. (2018). QuestPlus: a MATLAB implementation of the QUEST+ adaptive psychometric
#' method, Journal of Open Research Software, 6(1):27. doi: http://doi.org/10.5334/jors.195
#'
#' A. Turpin, P.H. Artes and A.M. McKendrick "The Open Perimetry Interface: An enabling tool for
#' clinical visual psychophysics", Journal of Vision 12(11) 2012.
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}
#'
#' @examples
#' chooseOpi("SimHenson")
#' if(!is.null(opiInitialize(type="C", cap=6)$err))
#'     stop("opiInitialize failed")
#'
#' #########################################################
#' # This section is for single location QUESTP
#' # This example fits a FoS curve
#' # Note: only fitting threshold and slope,
#' # modify the domain for FPR and FNR to fit those as well
#' #########################################################
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'     s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'               duration=200, responseWindow=1500, checkFixationOK=NULL)
#'     class(s) <- "opiStaticStimulus"
#'     return(s)
#' }
#'
#' #True parameters (variability is determined according to Henson et al. based on threshold)
#' loc <- list(threshold = 20, fpr = 0.05, fnr = 0.05)
#'
#' #Function to fit (Gaussian psychometric function)
#' pSeen <- function(x, params){return(params[3] +
#'                                         (1 - params[3] - params[4]) *
#'                                         (1 - pnorm(x, params[1], params[2])))}
#' #QUEST+
#' QP <- QUESTP(Fun = pSeen,
#'              stimDomain = list(0:50),
#'              paramDomain = list(seq(0, 40, 1), #Domain for the 50% threshold (Mean)
#'                                 seq(.5, 8, .5), #Domain for the slope (SD)
#'                                 seq(0.05, 0.05, 0.05), #Domain for the FPR (static)
#'                                 seq(0.05, 0.05, 0.05)), #Domain for the FNR (static)
#'              stopType="H", stopValue=4, maxPresentations=500,
#'              makeStim = makeStim,
#'              tt=loc$threshold, fpr=loc$fpr, fnr=loc$fnr,
#'              verbose = 2)
#'
#' #Plots results
#' #Henson's FoS function (as implemented in OPI - ground truth)
#' HensFunction <- function(Th){
#'     SD <- exp(-0.081*Th + 3.27)
#'     SD[SD > 6] <- 6
#'     return(SD)
#' }
#'
#' #Stimulus domain
#' dB_Domain <- 0:50
#' FoS <- pSeen(dB_Domain, params = QP$final) # Estimated FoS
#' FoS_GT <- pSeen(dB_Domain, params = c(loc$threshold, HensFunction(loc$threshold),
#'                                       loc$fpr, loc$fnr)) #Ground truth FoS (based on Henson et al.)
#'
#' #Plot (seen stimuli at the top, unseen stimuli at the bottom)
#' plot(dB_Domain, FoS_GT, type = "l", ylim = c(0, 1), xlab = "dB", ylab = "% seen", col = "blue")
#' lines(dB_Domain, FoS, col = "red")
#' points(QP$respSeq$stimuli, QP$respSeq$responses, pch = 16,
#'        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
#' legend("top", inset = c(0, -.2),legend = c("True","Estimated","Stimuli"),
#'        col=c("blue", "red","red"), lty=c(1,1,0),
#'        pch = c(16, 16, 16), pt.cex = c(0, 0, 1),
#'        horiz = TRUE, xpd = TRUE, xjust = 0)
#'
#' if (!is.null(opiClose()$err))
#'   warning("opiClose() failed")
#'
#'
#'
#' chooseOpi("SimHenson")
#' if(!is.null(opiInitialize(type="C", cap=6)$err))
#'     stop("opiInitialize failed")
#'
#' ######################################################################
#' # This section is for single location QUESTP
#' # This example shows that QUEST+ can replicate a ZEST procedure
#' # by fitting a FoS curve with fixed Slope, FPR and FNR
#' # Compared with ZEST
#' # Note that QUEST+ should be  marginally more efficient in selecting
#' # the most informative stimulus
#' ######################################################################
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'     s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'               duration=200, responseWindow=1500, checkFixationOK=NULL)
#'     class(s) <- "opiStaticStimulus"
#'     return(s)
#' }
#'
#' #True parameters (variability is determined according to Henson et al. based on threshold)
#' loc <- list(threshold = 30, fpr = 0.03, fnr = 0.03)
#'
#' #Function to fit (Gaussian psychometric function - Fixed slope (same as default likelihood in ZEST))
#' pSeen <- function(domain, tt){{0.03+(1-0.03-0.03)*(1-pnorm(domain,tt,1))}}
#'
#' # ZEST-like QUEST+ procedure
#' QP <- QUESTP(Fun = pSeen,
#'              stimDomain = list(0:40),
#'              paramDomain = list(seq(0, 40, 1)),
#'              stopType="S", stopValue=1.5, maxPresentations=500,
#'              makeStim = makeStim,
#'              tt=loc$threshold, fpr=loc$fpr, fnr=loc$fnr,
#'              verbose = 2)
#'
#' # ZEST
#' ZE <- ZEST(domain = 0:40,
#'            stopType="S", stopValue=1.5, maxPresentations=500,
#'            makeStim = makeStim,
#'            tt=loc$threshold, fpr=loc$fpr, fnr=loc$fnr,
#'            verbose = 2)
#'
#' #Plots results
#' #Henson's FoS function (as implemented in OPI - ground truth)
#' HensFunction <- function(Th){
#'     SD <- exp(-0.081*Th + 3.27)
#'     SD[SD > 6] <- 6
#'     return(SD)
#' }
#'
#' #Stimulus domain
#' dB_Domain <- 0:50
#' FoS_QP <- pSeen(domain = dB_Domain, tt = QP$final) # Estimated FoS
#' FoS_ZE <- pSeen(domain = dB_Domain, tt = ZE$final) # Estimated FoS
#'
#' #Plot (seen stimuli at the top, unseen stimuli at the bottom)
#' plot(dB_Domain, FoS_QP, type = "l", ylim = c(0, 1), xlab = "dB", ylab = "% seen", col = "blue")
#' lines(dB_Domain, FoS_ZE, col = "red")
#' points(QP$respSeq$stimuli, QP$respSeq$responses, pch = 16,
#'        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
#' points(ZE$respSeq[1,], ZE$respSeq[2,], pch = 16,
#'        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
#' legend("bottomleft", legend = c("QUEST+","ZEST","Stimuli QUEST+", "Stimuli ZEST"),
#'        col=c("blue", "red","blue","red"), lty=c(1,1,0,0),
#'        pch = c(16, 16, 16, 16), pt.cex = c(0, 0, 1, 1),
#'        horiz = FALSE, xpd = TRUE, xjust = 0)
#' abline(v = loc$threshold, lty = "dashed")
#'
#' if (!is.null(opiClose()$err))
#'   warning("opiClose() failed")
#'
#'
#' chooseOpi("SimHenson")
#' if(!is.null(opiInitialize(type="C", cap=6)$err))
#'   stop("opiInitialize failed")
#'
#' #########################################################
#' # This section is for single location QUESTP
#' # This example fits a broken stick spatial summation function
#' # with a multi-dimensional stimulus (varying in size and intensity).
#' # Stimulus sizes are limited to GI, GII, GIII, GIV and GV.
#' # The example also shows how to use a helper function to
#' # simulate responses to multi-dimensional stimuli
#' # (here, the simulated threshold varies based on stimulus size)
#' #########################################################
#' makeStim <- function(stim, n) {
#'   s <- list(x=9, y=9, level=dbTocd(stim[1]), size=stim[2], color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#'
#' # Helper function for true threshold (depends on log10(stimulus size),
#' # diameter assumed to be the second element of stim vector)
#' ttHelper_SS <- function(location) {  # returns a function of (stim)
#'   ff <- function(stim) stim
#'
#'   body(ff) <- substitute(
#'     {return(SensF(log10(pi*(stim[2]/2)^2), c(location$Int1, location$Int2, location$Slo2)))}
#'   )
#'   return(ff)
#' }
#'
#' # Function of sensivity vs SSize (log10(stimulus area))
#' SensF <- function(SSize, params){
#'   Sens <- numeric(length(SSize))
#'   for (i in 1:length(SSize)){
#'     Sens[i] <- min(c(params[1] + 10*SSize[i], params[2] + params[3]*SSize[i]))
#'   }
#'   Sens[Sens < 0] <- 0
#'   return(Sens)
#' }
#'
#' Sizes <- c(0.1, 0.21, 0.43, 0.86, 1.72)
#'
#' #True parameters (variability is determined according to Henson et al. based on threshold)
#' loc <- list(Int1 = 32, Int2 = 28, Slo2 = 2.5, fpr = 0.05, fnr = 0.05, x = 9, y = 9)
#'
#'
#' # Function to fit (probability of seen given a certain stimulus intensity and size,
#' # for different parameters)
#' pSeen <- function(stim, params){
#'
#'   Th <- SensF(log10(pi*(stim[2]/2)^2), params)
#'
#'   return(0.03 +
#'            (1 - 0.03 - 0.03) *
#'            (1 - pnorm(stim[1], Th, 1)))
#' }
#'
#'
#' \dontrun{
#' set.seed(111)
#' #QUEST+ - takes some time to calculate likelihoods
#' QP <- QUESTP(Fun = pSeen,
#'              stimDomain = list(0:50, Sizes),
#'              paramDomain = list(seq(0, 40, 1), # Domain for total summation intercept
#'                                 seq(0, 40, 1), # Domain for partial summation intercept
#'                                 seq(0, 3, 1)), # Domain for partial summation slope
#'              stopType="H", stopValue=1, maxPresentations=500,
#'              makeStim = makeStim,
#'              ttHelper=ttHelper_SS(loc), tt = 30,
#'              fpr=loc$fpr, fnr=loc$fnr,
#'              verbose = 2)
#' #Stimulus sizes
#' G <- log10(c(pi*(0.1/2)^2, pi*(0.21/2)^2, pi*(0.43/2)^2, pi*(0.86/2)^2, pi*(1.72/2)^2));
#' SizesP <- seq(min(G), max(G), .05)
#'
#' # True and estimated response
#' Estim_Summation <- SensF(SizesP, params = QP$final) # Estimated spatial summation
#' GT_Summation <- SensF(SizesP, params = c(loc$Int1, loc$Int2, loc$Slo2)) # True spatial summation
#'
#' #Plot
#' plot(10^SizesP, GT_Summation, type = "l", ylim = c(0, 40), log = "x",
#'      xlab = "Stimulus area (deg^2)", ylab = "Sensitivity (dB)", col = "blue")
#' lines(10^SizesP, Estim_Summation, col = "red")
#' points(pi*(QP$respSeq$stimuli.2/2)^2, QP$respSeq$stimuli.1, pch = 16,
#'        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3))
#' legend("top", inset = c(0, -.2),legend = c("True","Estimated","Stimuli"),
#'        col=c("blue", "red","red"), lty=c(1,1,0),
#'        pch = c(16, 16, 16), pt.cex = c(0, 0, 1),
#'        horiz = TRUE, xpd = TRUE, xjust = 0)
#'
#' }
#' if (!is.null(opiClose()$err))
#'   warning("opiClose() failed")
#'
#' @export
QUESTP <- function(Fun, #function to be fitted, needs to output a probability of seen
                   stimDomain,
                   paramDomain,
                   likelihoods = NULL,
                   priors = NULL,
                   stopType="H",
                   stopValue=4,
                   maxSeenLimit=2,
                   minNotSeenLimit=2,
                   minPresentations=1,
                   maxPresentations=100,
                   minInterStimInterval=NA,
                   maxInterStimInterval=NA,
                   verbose=0, makeStim,
                   ...) {
    ##########################
    # Validate params
    ##########################

    #Creates a QUESTP element
    state <- QUESTP.start(Fun = Fun, stimDomain = stimDomain,
                          paramDomain = paramDomain, likelihoods = likelihoods, priors = priors,
                          stopType=stopType, stopValue=stopValue, maxSeenLimit=maxSeenLimit,
                          minNotSeenLimit=minNotSeenLimit, minPresentations=minPresentations,
                          maxPresentations=maxPresentations,
                          makeStim=makeStim,...)
    pdfs <- NULL
    #Steps until stopping criterion
    while (!QUESTP.stop(state)) {
        r <- QUESTP.step(state)
        state <- r$state
        if (verbose == 2) {
            cat(sprintf("Presentation %2d: ", state$numPresentations))
            cat(sprintf("stim= %5s response=%s ", paste0(as.numeric(utils::tail(state$stimuli,1)), collapse = ", "),
                        utils::tail(state$responses,1)))
            cat(sprintf("fixation= %1.0g ", utils::tail(state$fixated,1)))
            cat(sprintf("stdev= %8.4g H= %8.4g\n", QUESTP.stdev(state), QUESTP.entropy(state)))
        }
        if (verbose > 0)
            pdfs <- c(pdfs, list(state$pdf))

        if (!is.na(minInterStimInterval) && !is.na(maxInterStimInterval))
            Sys.sleep(stats::runif(1, min = minInterStimInterval, max = maxInterStimInterval)/1000)
    }

    return(list(
        npres = utils::tail(state$numPresentations,1),        # number of presentations
        respSeq = data.frame(ID = paste0(as.numeric(utils::tail(state$stimuli,1)), collapse = ", "),
                           responses = state$responses,
                           fixated = state$fixated,
                           stimuli = state$stimuli), # response sequence (list of triples)
        pdfs = pdfs,                                   # list of pdfs used (if verbose > 0)
        final = QUESTP.final(state),                     # final threshold estimate
        opiResp = state$opiResp                        # list of all responses from opiPresent
    ))
}#QUESTP

# Sets up priors for the parameters
#' @rdname QUESTP
#' @export
QUESTP.Prior <- function(state, priors = NULL){

    if (is.null(priors)){
        n <- dim(state$paramDomain)[1]
        state$priorsP <- list()
        for (i in 1:n){
            nn <- length(unique(state$paramDomain[i,]))
            state$priorsP[[i]] <- (numeric(nn) + 1)/nn
        }
    }else{
        state$priorsP <- priors
    }

    #Combined prior (assumes independence)
    state$prior <- apply(combvec(state$priorsP),2,prod)
    state$prior <- state$prior/sum(state$prior)
    state$pdf <- state$prior

    return(state)
}

#Calculates likelihood functions (time consuming) - no need to export
#' @rdname QUESTP
#' @export
QUESTP.Likelihood <- function(state){
    state$likelihoods <- array(NA, c(dim(state$stimDomain)[2], dim(state$paramDomain)[2], length(state$respDomain)))

    for (i in 1:dim(state$stimDomain)[2]){
        for (j in 1:dim(state$paramDomain)[2]){
            y <- state$Fun(state$stimDomain[,i], state$paramDomain[,j]);
            state$likelihoods[i,j,] = c(1-y, y); # always assumes function provides probability of seen for a binary response
        }
    }
    return(state)
}

# Starts a QUESTP element
#' @rdname QUESTP
#' @export
QUESTP.start <- function(Fun, stimDomain, paramDomain, likelihoods = NULL, priors = NULL,
                         stopType="H", stopValue=4, maxSeenLimit = 2, minNotSeenLimit = 2,
                         minPresentations = 1, maxPresentations = 100, makeStim, ...){

    respDomain <- c(0, 1) #binary response, the only implemented for now

    #Stimulus and parameter domains need to be lists, one element for each dimension
    if (!is.list(stimDomain)){stimDomain <- list(stimDomain)}
    if (!is.list(paramDomain)){paramDomain <- list(paramDomain)}

    #Creates a QUESTP element
    state <- list(name="QUESTP",
                  Fun = Fun,                          # parametric function to fit
                  stimDomain = combvec(stimDomain),   # stimulus domain (can be multidimensional)
                  paramDomain = combvec(paramDomain), # parameter domain (can be multidimensional)
                  respDomain = respDomain,
                  likelihoods = likelihoods,          # pre-computed likelihoods, if available
                  priors = priors,
                  stopType=stopType,                  # stopping criterion
                  stopValue=stopValue,                # stopping value for the criterion
                  maxSeenLimit=maxSeenLimit,          # sets a max for number of presentations
                  minNotSeenLimit=minNotSeenLimit,    # sets a max for number of presentations
                  minPresentations=minPresentations,  # sets minimum num of presentations
                  maxPresentations=maxPresentations,  # sets max num of presentations
                  makeStim=makeStim,                  # stimulus helper function
                  currSeenLimit=0,                    # number of times maxStimulus seen
                  currNotSeenLimit=0,                 # number of times minStimulus not seen
                  numPresentations=0,                 # number of presentations so far
                  stimuli=NULL,                       # vector of stims shown
                  responses=NULL,                     # vector of responses (1 seen, 0 not)
                  responseTimes=NULL,                 # vector of response times
                  fixated=NULL,                       # vector of true/false for fixated one per stimuli
                  opiResp=NULL,                       # list of opiPresent return values
                  opiParams=list(...)                 # the extra params
    )

    #Calculates likelihoods (slow)
    if (is.null(likelihoods)){
        #Calculated if no pre-computed likelihoods are provided (slow)
        state <- QUESTP.Likelihood(state)
    }else{
        state.likelihoods = likelihoods
    }

    #Initializes priors
    state <- QUESTP.Prior(state, priors)
    return(state)
}

#Gets the optimal stimulus to minimize expected entropy - useful to export for nextStim implementation
#' @rdname QUESTP
#' @importFrom Rfast rowsums colAll
#' @importFrom abind abind
#' @export
getTargetStim <- function(state){
    # Compute the product of likelihood and current
    # pdf array, at each outcome and stimulus
    # location.
    postTimesL <- aperm(state$pdf*
                            aperm(state$likelihoods,
                                  c(2, 1, 3)), c(2, 1, 3)) #this replicates matlab element-wise multiplication

    # Compute the (total) probability of each outcome at each
    # stimulus location.

    #pk <- apply(postTimesL, c(1,3), sum)
    pk <- cbind(Rfast::rowsums(postTimesL[, , 1]), Rfast::rowsums(postTimesL[, , 2]))

    #Compute new pdf PDFs, by normalising values so that
    #they sum to 1
    newpdfs <- abind::abind(postTimesL[, , 1] / pk[, 1], postTimesL[, , 2] / pk[, 2], along = 3)

    # Compute the the entropy that would result from outcome r at stimulus x,

    #### This is a slower implementation without RFast
    #H <- -apply(newpdfs * log(newpdfs), c(1,3), sum, na.rm = TRUE)
    ##################################################

    H <- newpdfs * log(newpdfs)
    H[is.na(H)] <- 0
    H <- -cbind(Rfast::rowsums(H[,,1]), Rfast::rowsums(H[,,2]))

    # Compute the expected entropy for each stimulus
    # location, summing across all responses (Dim 3)

    #### This is a slower implementation without RFast
    #EH <- apply(pk*H, 1, sum)
    ##################################################

    EH <- Rfast::rowsums(pk*H)

    # Find the index of the stimulus with the smallest
    # expected entropy.
    idx <- which.min(EH);

    #Stimulus
    stim <- state$stimDomain[,idx]

    return(stim)
}

#' @rdname QUESTP
#' @param state Current state of the QUESTP returned by \code{QUESTP.start} and \code{QUESTP.step}.
#' @param nextStim A valid object for \code{opiPresent} to use as its \code{nextStim}.
#' @export
QUESTP.step <- function(state, nextStim = NULL) {

    #Gets the most informative stimulus - this is time consuming for large domains/many dimensions
    stimIndex <- which(Rfast::colAll(state$stimDomain == getTargetStim(state)))
    stim <- state$stimDomain[,stimIndex]

    #stim <- getTargetStim(state)

    if (length(stim) > 1 && substr(opiQueryDevice()$machine, 1, 3) == "Sim") {
        if (is.function(state$opiParams$ttH)){
            state$opiParams$tt <- max(state$opiParams$ttH(stim), 0)
        }else{
            stop("For multi-dimensional stimuli,
                 OPI needs a helper function for the stimulus
                 in simulation mode - how does
                 the threshold change with different stimulus parameters?")
        }
    }
    #

    params <- c(list(stim=state$makeStim(stim, state$numPresentations), nextStim=nextStim), state$opiParams)
    opiResp <- do.call(opiPresent, params)
    while (!is.null(opiResp$err))
        opiResp <- do.call(opiPresent, params)

    fixation_is_good <- TRUE
    if (!is.null(params$stim$checkFixationOK)) {
        fixation_is_good <- params$stim$checkFixationOK(opiResp)
    }

    state$stimuli          <- rbind(state$stimuli, stim)
    state$responses        <- c(state$responses, opiResp$seen)
    state$responseTimes    <- c(state$responseTimes, opiResp$time)
    state$numPresentations <- state$numPresentations + 1
    state$fixated          <- c(state$fixated, fixation_is_good)
    state$opiResp          <- c(state$opiResp, list(opiResp))

    if (fixation_is_good) {  # update the pdf

        respIndex <- which(opiResp$seen == state$respDomain)
        w <- 1
        # Update pdf PDF (assuming trial-by-trial independence)
        w_likelihood <- w*(state$likelihoods[stimIndex,,respIndex] - .5) + .5
        state$pdf <- state$pdf * w_likelihood

        state$pdf <- state$pdf/sum(state$pdf)
    } else {
        warning("QUESTP.step: fixation lost during presentation, pdf not updated")
    }

    return(list(state=state, resp=opiResp))
}#QUESTP.step()

#' @rdname QUESTP
#' @export
# Evaluates stopping criterion
QUESTP.stop <- function(state) {
    keepGoing <- (
        (state$numPresentations < state$maxPresentations) &&
            (state$currNotSeenLimit < state$minNotSeenLimit) &&
            (state$currSeenLimit    < state$maxSeenLimit) &&
            (
                ((state$stopType == "S") && (QUESTP.stdev(state) > state$stopValue))
                || ((state$stopType == "H") && (QUESTP.entropy(state) > state$stopValue))
                || ((state$stopType == "N") && (state$numPresentations < state$stopValue))
            ) ||
            (state$numPresentations < state$minPresentations)
    )
    return (!keepGoing)
}#QUESTP.stop

#' @rdname QUESTP
#' @export
QUESTP.final <- function(state, Choice = "mean") {
    if (Choice == "mean") {
        final <- colSums(state$pdf*t(state$paramDomain))
    } else if (Choice == "mode") {
        final <- state$paramDomain[,which.max(state$pdf)]
    } else if (Choice == "median") {
        final <- state$paramDomain[,which.min(abs(cumsum(state$pdf) - 0.5))]
    }

    return(final)
}#QUESTP.final

#' @rdname QUESTP
#' @export
# Calculates standard deviation
# this only really makes sense for one-dimensional parameter domains.
# If multi-dimensional, calculates sd for each parameter and returns the highest value
# unless a specific parameter is specified by WhichP
# (may be useful to monitor a specific parameter of interest such as threshold)
QUESTP.stdev <- function(state, WhichP = NULL){
    SDs <- numeric(dim(state$paramDomain)[1])
    for (i in 1:length(SDs)){
        if (length(unique(state$paramDomain[i,])) < 2){next}
        SDs[i] <- sqrt(sum(state$pdf * state$paramDomain[i,] * state$paramDomain[i,], na.rm = TRUE) -
                           sum(state$pdf * state$paramDomain[i,], na.rm = TRUE)^2)
    }
    if (is.null(WhichP)){
        return(max(SDs))
    }else{
        return(SDs[i])
    }
}

#' @rdname QUESTP
#' @export
#Calculates entropy
QUESTP.entropy <- function(state){
    H <- -sum(state$pdf * log2(state$pdf), na.rm = TRUE)
    return(H)
}

#
# Perform a ZEST procedure at each location in a 24-2 pattern (right eye)
# using the HFA growth pattern to set the priors at each location.
#
# The prior pdf at each location is bimodal, with the guess from the HFA growth
# pattern as the mode of the normal part of the pdf, and the damaged part
# is fixed. The guess for the primary points (+-9,+-9) is passed in as a
# parameter to runBiModalZEST().
#
# The domain of the prior is -5:30 dB, which you might want to change.
#
# See example usage at end of file.
# Warning: set max stim brightness in call to dbTocd()
#
# Author: Andrew Turpin & Luke Chong
# Date: Wed 12 Jun 2013 13:06:22 EST
#
# Modified Tue  1 Jul 2014: Allow for tt=NA (ie not simulator mode)
# Modified Tue 30 Jan 2015: Added max stim brightness in dbTocd(...)
#

source("growthPattern.r")

#########################################################################
# INPUTS:
#   eye               - "right" or "left"
#   primaryStartValue - the mode of the normal part of the prior pdf for
#                       locations (+-9,+-9).
#   If running in simulation mode you need to specify 
#     tt - an 8*9 matrix 24-2 field OD (blind spot on right) 
#     fpv - false positive rate in range [0,1]
#     fnv - false negative rate in range [0,1]
#   
# RETURNS: a list with
#    np = matrix of number of presenations for each location
#    ae = matrix of absolute error for each location
#    th = matrix of measured thresholds for each location
#    trues = matrix of input thresholds
#
# ALGORITHMS: Makes use of procedureWithGrowthPattern(...).
#########################################################################
Zest242 <- function(eye="right", primaryStartValue=30, 
                            tt=NA, fpv=0.00, fnv=0.00) {
    ####################################################################
    # Each location derives its start value from the average of all of the
    # immediate 9 neighbours that are lower than it.
    # Numbers should start at 1 and increase, not skipping any.
    ####################################################################
    if (eye == "right") {
        growthPattern <- matrix(c(
            NA, NA, NA,  3,  3,  3,  3, NA, NA,
            NA, NA,  2,  2,  2,  2,  2,  2, NA,
            NA,  3,  2,  1,  2,  2,  1,  2,  3,
             4,  3,  2,  2,  2,  2,  2, NA,  3,
             4,  3,  2,  2,  2,  2,  2, NA,  3,
            NA,  3,  2,  1,  2,  2,  1,  2,  3,
            NA, NA,  2,  2,  2,  2,  2,  2, NA,
            NA, NA, NA,  3,  3,  3,  3, NA, NA
        ), nrow=8, ncol=9, byrow=TRUE)
            # starting guesses for the "1" locations
        p <- primaryStartValue
        onePriors <- matrix(c(
             NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA,  p, NA, NA,  p, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA,  p, NA, NA,  p, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA
        ), nrow=8, ncol=9, byrow=TRUE)
    } else {
        if (eye != "left")
            warning("Zest242(): assuming left eye")
        growthPattern <- matrix(c(
           NA, NA,  3,  3,  3,  3, NA, NA, NA, 
           NA,  2,  2,  2,  2,  2,  2, NA, NA, 
            3,  2,  1,  2,  2,  1,  2,  3, NA, 
            3, NA,  2,  2,  2,  2,  2,  3,  4, 
            3, NA,  2,  2,  2,  2,  2,  3,  4, 
            3,  2,  1,  2,  2,  1,  2,  3, NA, 
           NA,  2,  2,  2,  2,  2,  2, NA, NA, 
           NA, NA,  3,  3,  3,  3, NA, NA, NA 
        ), nrow=8, ncol=9, byrow=TRUE)
            # starting guesses for the "1" locations
        p <- primaryStartValue
        onePriors <- matrix(c(
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA,  p, NA, NA,  p, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA,  p, NA, NA,  p, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, NA
        ), nrow=8, ncol=9, byrow=TRUE)
    }

        # check tt is in the right format
    if (is.matrix(tt)) {
        if (ncol(tt) != 9 || nrow(tt) != 8)
            stop("Zest242(): tt is not an 8*9 matrix")
        z1 <- !is.na(growthPattern)
        z2 <- !is.na(tt)
        if (!all((z1 & z2) == z1))
            stop(paste("Zest242(): tt matrix is not the correct style for a",eye,"eye"))
    }

    ###############################
    # Set up fp and fn matrices, and tt matrix if not supplied
    ###############################
    z <- !is.na(growthPattern)
    fp <- z * matrix(fpv, nrow(growthPattern), ncol(growthPattern))
    fn <- z * matrix(fnv, nrow(growthPattern), ncol(growthPattern))
    if (is.na(tt))
        tt <- matrix(NA, nrow(growthPattern), ncol(growthPattern))

    #####################################################
    # Given guess, create a state for location (rw, cl)
    # This version assumes
    #  1) a 24-2 pattern, so 
    #        for right eye: (rw, cl) -> (27 - 6*rw, -27 + 6*cl).
    #        for left eye:  (rw, cl) -> (33 - 6*rw, -27 + 6*cl).
    #  2) bi-modal prior, constructed as in 
    #     Turpin et al  IOVS 44(11), November 2003. Pages 4787-4795.
    #  3) a domain of -5:30 dB
    #####################################################
    startF <- function(guess, rw, cl) {
        domain <- -5:30 # note min(domain) should be <= 0 for bimodal pdf

        glaucomaPDF <- rep(0.001,length(domain))
        glaucomaPDF[1:(6+abs(domain[1]))] <- c(rep(0.001,abs(domain[1])),0.2, 0.3, 0.2, 0.15,0.1, 0.02)
        healthyPDF  <- function (normalModePoint) {
            temp <- c(rep(0.001,50), 0.009, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01,rep(0.001,50))
            mode <- which.max(temp)
            return(temp[(mode-normalModePoint+domain[1]):(mode-normalModePoint+domain[length(domain)])])
        }
        makeBiModalPDF <- function(normalModePoint, weight, pdf.floor) {
          npdf <- healthyPDF(normalModePoint)
          cpdf <- npdf * weight + glaucomaPDF
          cpdf[which(cpdf < pdf.floor)] = pdf.floor 
          return (cpdf)
        }	
        prior <- makeBiModalPDF(round(guess),4,0.001)
        prior <- prior/sum(prior)
      
        ###### helps create the makeStim function needed for the OPI
        makeStimHelper <- function(x, y) {  # returns a function of (db,n)
            ff <- function(db, n) db+n

            body(ff) <- substitute(
                {s <- list(x=x, y=y, level=dbTocd(db,4000/pi), size=0.43, color="white",
                        duration=200, responseWindow=1500)
                class(s) <- "opiStaticStimulus"
                return(s)
                }
                , list(x=x,y=y))
            return(ff)
        }

        if (eye == "right")
            ms <- makeStimHelper(27 - 6*rw, -27 + 6*cl)    
        else
            ms <- makeStimHelper(6+27 - 6*rw, -27 + 6*cl)    
        return(ZEST.start(domain=domain, prior=prior, makeStim=ms, 
                    tt=tt[rw,cl],
                    fpr=fp[rw,cl],
                    fnr=fn[rw,cl]
        ))
    }

    #####################################################
    # Given a state, step the procedure and return new state
    #####################################################
    stepF <- function(state) { return(ZEST.step(state)$state) } 

    #####################################################
    # Given a state, return TRUE for finished, FALSE otherwise
    #####################################################
    stopF <- ZEST.stop

    #####################################################
    # Given a state, return c(threshold, num presentations)
    # Round -ve thresholds to 0
    #####################################################
    finalF <- function(state) {
        t <- ZEST.final(state)
        return(c(t, state$numPresentations))
    }

    res1 <- procedureWithGrowthPattern(growthPattern, onePriors, startF, stepF, stopF, finalF)
    z <- res1$t < 0
    tz <- res1$t
    tz[z] <- 0
    res <- list(np=res1$n, ae=abs(tz-tt), th=res1$t, trues=tt)

    return(res)
}

####################################
# Example Usage
####################################
##require(OPI)
####chooseOpi("SimYes")
##chooseOpi("SimNo")
###chooseOpi("SimHenson")
##opiInitialise()
###
###tt <- matrix(c(
###   NA, NA,  23,  23,  23,  23, NA, NA, NA, 
###   NA, 24,  24,  24,  24,  24, 24, NA, NA, 
###   23, 24,  25,  24,  24,  25, 24, 23, NA, 
###   23, NA,  24,  24,  24,  24, 24, 23,  4, 
###   23, NA,  24,  24,  24,  24, 24, 23,  4, 
###   23, 24,  25,  24,  24,  25, 24, 23, NA, 
###   NA, 24,  24,  24,  24,  24, 24, NA, NA, 
###   NA, NA,  23,  23,  23,  23, NA, NA, NA 
###), nrow=8, ncol=9, byrow=TRUE)
###
##print(Zest242(eye="left", primaryStartValue=25, tt=tt, fpv=0, fnv=0))

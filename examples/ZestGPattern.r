#
# Perform a ZEST procedure at each location in a G20 pattern (right eye)
# using a custom growth pattern to set the priors at each location.
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

    # G20 pattern for a right eye
xys <- matrix(c(
 -8,  26, 8,  26, -20,  20, -12,  20, -4,  20, 4,  20, 12,  20, 20,  20, -4,  14,
  4,  14, -20,  12, -12,  12, 12,  12, 20,  12, -8,   8, -2,   8, 2,   8, 8,   8,
 26,   8, -26,   4, -20,   4, -14,   4, -4,   4, 4,   4, 22,   4, -8,   2, -2,   2,
  2,   2, 8,   2, 0,   0, -8,  -2, -2,  -2, 2,  -2, 8,  -2, -26,  -4, -20,  -4, -14,  -4,
 -4,  -4, 4,  -4, 22,  -4, -8,  -8, -3,  -9, 3,  -9, 8,  -8, 26,  -8, -20, -12,
-12, -12, 12, -12, 20, -12, -4, -14, 4, -14, -20, -20, -12, -20, -4, -20, 4, -20,
 12, -20, 20, -20, -8, -26, 8, -26), 
ncol=2, byrow=TRUE)


    # These locations will start with primaryStartValue in ZestG20()
    # Indexes into rows of xys
starters <- c(15, 18, 41, 44)

    # When one of these locations finishes, it will 
    # add starting states for all of its not-already-started 
    # children. The child starting values will be the average of 
    # any completed parents at that point in time (see growthPatternParents.r)
    # Numbers are indexes into rows of xys.
kids <- vector('list',  59)
kids[[4]] <- c(1,3)
kids[[5]] <- c(1)
kids[[6]] <- c(2)
kids[[7]] <- c(2,8)
kids[[9]] <- c(5)
kids[[10]] <- c(6)
kids[[11]] <- c(3,20)
kids[[12]] <- c(11, 4, 21)
kids[[13]] <- c(7, 14)
kids[[14]] <- c(8, 19, 25)
kids[[15]] <- c(12, 22, 26, 23, 16, 9)
kids[[18]] <- c(10, 17, 24, 29, 13)
kids[[21]] <- c(20)
kids[[22]] <- c(11,21)
kids[[23]] <- c(27)
kids[[24]] <- c(28)
kids[[27]] <- c(30)
kids[[28]] <- c(30)
kids[[32]] <- c(30)
kids[[33]] <- c(30)
kids[[36]] <- c(35)
kids[[37]] <- c(36, 46)
kids[[38]] <- c(32)
kids[[39]] <- c(33)
kids[[41]] <- c(31,37,47,50,42,38)
kids[[44]] <- c(34,39,43,51,48)
kids[[46]] <- c(35)
kids[[47]] <- c(46, 52, 53)
kids[[48]] <- c(56, 49, 57)
kids[[49]] <- c(40,45)
kids[[50]] <- c(53, 54)
kids[[51]] <- c(55,56)
kids[[53]] <- c(58)
kids[[54]] <- c(58)
kids[[55]] <- c(59)
kids[[56]] <- c(59)

#########################################################################
# INPUTS:
#   eye               - "right" or "left"
#   primaryStartValue - the mode of the normal part of the prior pdf for
#                       locations 15, 18, 41 and 44
#   If running in simulation mode you need to specify 
#     tt - a vector of 59 true thresholds
#     fpv - false positive rate in range [0,1]
#     fnv - false negative rate in range [0,1]
#   
# RETURNS: a list with
#    np = vector of number of presenations for each location
#    th = vector of measured thresholds for each location
#
# ALGORITHMS: Makes use of procedureWithGeneralGrowthPattern(...).
#########################################################################
ZestG20 <- function(eye="right", primaryStartValue=30, 
                    tt=NULL, fpv=0.00, fnv=0.00) {
    if (!is.null(tt) && length(tt) != 59)
        stop(paste("ZestG20() expects 59 true threholds, or NULL"))

    if (eye == "left") 
        xys[,1] <- xys[,1] * -1

    #####################################################
    # Given guess, create a state for location loc_number
    # This version assumes
    #  1) bi-modal prior, constructed as in 
    #     Turpin et al  IOVS 44(11), November 2003. Pages 4787-4795.
    #  2) a domain of -5:40 dB
    #####################################################
    startF <- function(guess, loc_number) {
        if (is.na(guess))
            guess <- primaryStartValue

        domain <- -5:40 # note min(domain) should be <= 0 for bimodal pdf

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
        makeStimHelper <- function(x,y) {  # returns a function of (db,n)
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
            ms <- makeStimHelper(xys[loc_number, 1],xys[loc_number, 2])    
        else
            ms <- makeStimHelper(-xys[loc_number, 1],xys[loc_number, 2])    

        return(ZEST.start(domain=domain, prior=prior, makeStim=ms, 
                    tt=tt[loc_number],
                    fpr=fpv,
                    fnr=fnv
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

    res1 <- procedureWithGeneralGrowthPattern(
                starters, kids, 
                startF, stepF, stopF, finalF, 
                waitForAllParents=FALSE)
    z <- res1$mt < 0
    tz <- res1$mt
    tz[z] <- 0
    res <- list(np=res1$n, mt=tz)

    return(res)
}

####################################
# Example Usage
####################################
###require(OPI)
#####chooseOpi("SimYes")
######chooseOpi("SimNo")
###chooseOpi("SimHenson")
###opiInitialise()
###
###tt <- 0:58
###print(z <- ZestG20(eye="right", primaryStartValue=25, tt=tt, fpv=0, fnv=0))
###barplot(z$mt - tt)

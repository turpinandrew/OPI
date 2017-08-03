#
# Perform a FT procedure at each location in a 24-2 pattern (right eye)
# using the HFA growth pattern to set the priors at each location.
#
# The prior pdf at each location is bimodal, with the guess from the HFA growth
# pattern as the mode of the normal part of the pdf, and the damaged part
# is fixed. The guess for the primary points (+-9,+-9) is passed in as a
# parameter to runBiModalFT().
#
# The domain of the prior is -5:30 dB, which you might want to change.
#
# See example usage at end of file. (Untested on Octopus 900.)
#
# Author: Andrew Turpin
# Date: Thu 19 Sep 2013 13:16:23 EST
#

source("growthPattern.r")

#########################################################################
# INPUTS:
#   eye               - "right" or "left"
#   primaryStartValue - the mode of the normal part of the prior pdf for
#                       locations (+-9,+-9).
#   If running in simulation mode you need to specify 
#     tt - an 8x9 matrix 24-2 field OD (blind spot on right) 
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
FT242 <- function(eye="right", primaryStartValue=25, 
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
      warning("FT242(): assuming left eye")
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
      stop("FT242(): tt is not an 8x9 matrix")
    z1 <- !is.na(growthPattern)
    z2 <- !is.na(tt)
    if (!all((z1 & z2) == z1))
      stop(paste("FT242(): tt matrix is not the correct style for a",eye,"eye"))
  }
  
  ###############################
  # Set up fp and fn matrices
  ###############################
  z <- !is.na(growthPattern)
  fp <- z * matrix(fpv, nrow(growthPattern), ncol(growthPattern))
  fn <- z * matrix(fnv, nrow(growthPattern), ncol(growthPattern))
  
  #####################################################
  # Given guess, create a state for location (rw, cl)
  # This version assumes
  #  1) a 24-2 pattern, so 
  #        for right eye: (rw, cl) -> (27 - 6*rw, -27 + 6*cl).
  #        for left eye:  (rw, cl) -> (33 - 6*rw, -27 + 6*cl).
  #  2) starting stim values are rounded to nearest int
  #####################################################
  startF <- function(guess, rw, cl) {
    ###### helps create the makeStim function needed for the OPI
    makeStimHelper <- function(x, y) {  # returns a function of (db,n)
      ff <- function(db, n) db+n
      
      body(ff) <- substitute(
        {s <- list(x=x, y=y, level=dbTocd(db, 4000/pi), size=0.43, color="white",
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
    
    return(FT.start(est=round(guess,0), makeStim=ms, 
                    tt=tt[rw,cl],
                    fpr=fp[rw,cl],
                    fnr=fn[rw,cl]
    ))
  }
  
  #####################################################
  # Given a state, step the procedure and return new state
  #####################################################
  stepF <- function(state) { return(FT.step(state)$state) } 
  
  #####################################################
  # Given a state, return TRUE for finished, FALSE otherwise
  #####################################################
  stopF <- FT.stop
  
  #####################################################
  # Given a state, return c(threshold, num presentations)
  #####################################################
  finalF <- function(state) {
    t <- FT.final(state)
    return(c(t, state$numPresentations))
  }
  
  res1 <- procedureWithGrowthPattern(growthPattern, onePriors, startF, stepF, stopF, finalF)
  res <- list(np=res1$n, ae=abs(res1$t-tt), th=res1$t, trues=tt)
  
  return(res)
}

####################################
# Example Usage
####################################
### require(OPI)
### #chooseOpi("SimYes")
### #chooseOpi("SimNo")
### chooseOpi("SimHenson")
### opiInitialise()
### 
### tt <- matrix(c(
###    NA, NA,  23,  23,  23,  23, NA, NA, NA, 
###    NA, 24,  24,  24,  24,  24, 24, NA, NA, 
###    23, 24,  25,  24,  24,  25, 24, 23, NA, 
###    23, NA,  24,  24,  24,  24, 24, 23,  4, 
###    23, NA,  24,  24,  24,  24, 24, 23,  4, 
###    23, 24,  25,  24,  24,  25, 24, 23, NA, 
###    NA, 24,  24,  24,  24,  24, 24, NA, NA, 
###    NA, NA,  23,  23,  23,  23, NA, NA, NA 
### ), nrow=8, ncol=9, byrow=TRUE)
### 
### print(FT242(eye="left", primaryStartValue=25, tt=tt, fpv=0, fnv=0))
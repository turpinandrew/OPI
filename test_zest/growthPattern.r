#
# Implements procedureWithGrowthPattern() which allows a general
# thresholding procedure to be run with a growth pattern where a
# subsset of locations must finish before the next subset begins 
# (eg as in the HFA I, II and III)
#
# The procedure is controlled by a growth pattern matrix, and
# requires a matrix of guesses for the first subset of locations, 
# and functions to start, step, test-for-stop, and get-final-threshold
# for the procedure. See comments and 242Zest.r for an example.
#
# Author: Andrew Turpin 
# Date: Mon 27 May 2013 18:23:57 EST
#

####################################################################
# Use startFun, stepFun and stopFun to run all locations with number
# 1 in the growthPattern, then number 2 (using the ones to set prior), 
# then number 3, using the 1s and 2s, and so on until locations
# max(growthPattern) are done.
# After each location finishes, use finalFun to get threshold and num pres.
# Within each subset of locations with the same growth pattern number,
# locations are presented in random order.
#
# INPUTS: 
#   gp  - growth pattern matrix (min(gp) == 1)
#   ops - matrix same size as gp giving start values for gp==1
#   startFun - function(guess, rw, cl) that creates a state for
#              location (rw, cl) in gp using guess as start guess
#   stepFun  - function(state) returns a new state after one presentation
#   stopFun  - function(state) returns TRUE if location finished
#   finalFun - function(state) returns c(final threshold, number presenations)
#
# RETURNS: list of two matrices, each with same dimensions as gp
#           t is final threshold at each location
#           n is number of presentations at each location
####################################################################
procedureWithGrowthPattern <- function(gp, ops, startFun, stepFun, stopFun, finalFun) {

    ####################################################################
    # Average all immediate 9 neighbours that have num less than "wave"
    #
    # INPUTS
    #   gp   - growth Pattern matrix
    #   wave - integer wave number
    #   rw   - row of location
    #   cl   - column of location
    #   currT - matrix of current thresholds (if wave == 1 then these should
    #                               have starting guesses for the 1 locations)
    #
    # RETURNS: start guess for location (rw, cl) which is average of
    #          neighbours that are less than "wave" in "gp"
    ####################################################################
    makeGuess <- function(gp, wave, rw, cl, currT) {
        if (wave == 1)
            return(currT[rw,cl])

        n <- summ <- 0
        for(ii in -1:1) 
        for(jj in -1:1) 
        if (ii != 0 || jj != 0) {
            trow <- rw + ii
            tcol <- cl + jj
            if (all(c(trow,tcol)) > 0 && 
                trow <= nrow(gp)      && 
                tcol <= ncol(gp)      && 
                !is.na(gp[trow,tcol]) && 
                gp[trow, tcol] < wave) {
                    summ<- summ + currT[trow, tcol]
                    n <- n + 1
            }
        }

        if (n == 0)
            warning(paste("Could not find neighbour for",rw,cl))

        return(summ/n)
    }

        # set up answers
    currentThresholds <- ops  # cheat and start them at ops
    currentNumPres    <- matrix(NA, nrow(gp), ncol(gp))

        # Give each (row, col) a unique number map[row, col]
    map <- gp
    numLoc <- 1
    for(i in 1:nrow(map))
        for(j in 1:ncol(map)) 
            if (!is.na(gp[i,j])) {
                map[i, j] <- numLoc        
                numLoc <- numLoc + 1
            }

    states <- vector("list", numLoc-1)  # indexed by map[row,col]

        # Now loop finishing 1s, start rest, etc
    wave <- 1
    while (wave <= max(gp, na.rm=TRUE)) {
            # Create a state for each "wave" location
            # and step it until done
        locs <- which(gp == wave, arr.ind=TRUE)
        locsToGo <- 1:nrow(locs)
        while (length(locsToGo) > 0) {
            i <- locsToGo[runif(1, min=1, max=length(locsToGo))]
            rw <- locs[i,1]
            cl <- locs[i,2]
            index <- map[rw,cl]

            if (is.null(states[[index]]))
                states[[index]] <- startFun(makeGuess(gp, wave, rw, cl, currentThresholds), rw, cl)

            states[[index]] <- stepFun(states[[index]])

            if (stopFun(states[[index]])) {
                    # fill in currentThresholds and remove from locs
                currentThresholds[rw,cl] <- finalFun(states[[index]])[1]
                currentNumPres[rw,cl]    <- finalFun(states[[index]])[2]
                locsToGo <- locsToGo[-which(locsToGo == i)]
            }
        }
        wave <- wave + 1
    }

    return(list(t=currentThresholds, n=currentNumPres))
}

#
# OPI for Octopus 900 
# 
# This would all have been nicer in an OO style, with each implementation
# being a subclass of an opi class, but I don't think it can be in R.
# The OPI standard doesn't want users employing exactly the same function 
# no matter what the underlying implementation, and so there cannot be 
# extra parameters to create method signatures for different classes.
# Similarly, some implementations use exactly the same method signatures,
# again which will confuse R, I think. Anyway, if I am wrong, sorry about that.
# What I've done (use a list of implementations and then use a global
# integer to index them) works and makes sense to the non-OO person.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
#
# Copyright 2012 Andrew Turpin and Jonathan Denniss
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
# Author: Andrew Turpin
#
# July 2012
# Modified Feb 2013: opiPresent returns only a single frame
# Modified Nov 2013: add Tony's many holed wheel
#

require(rJava)

###################################################################
# .Octopus900Env$octopusObject is the java Opi object set in opiInitialize
# .Octopus900Env$... are lots of colors and Fixation constants set in setupBackgroundConstants()
###################################################################
if (!exists(".Octopus900Env"))
    .Octopus900Env <- new.env()

###########################################################################
# Get values for fixation, color and bg intensity constants
# from EyeSuite classes, and set globals
#       .Octopus900Env$* 
# to the values of those constants.
# INPUT: None.
# OUTPUT: None.
# SIDE EFFECTS: sets .Octopus900Env$* if possible.
###########################################################################
setupBackgroundConstants <- function() {
    f <- .jfields("com.hs.eyesuite.ext.extperimetryviewer.peristatic.data.exam.Const")

        #
        # check if cName exists as a field in f. If so, set 
        # .Octopus900Env$cName <- class value for cName
        #
    getC <- function(cName) {
        if (length(grep(cName, f)) > 0) 
            assign(cName,
                   .jfield("com.hs.eyesuite.ext.extperimetryviewer.peristatic.data.exam.Const",NULL,cName), 
                   envir = .Octopus900Env)
    }

    getC("FIX_CENTRE")
    getC("FIX_CROSS")
    getC("FIX_RING")
    getC("BG_OFF")
    getC("BG_1")        # 1.27 cd/m2 == 127 passed to MsgInitializePerimUnit
    getC("BG_10")       # 10 cd/m2 == 1000
    getC("BG_100")      # 100 cd/m2 == 10000

    assign("FIX_CENTER", .Octopus900Env$FIX_CENTRE, envir = .Octopus900Env) # help Americans

    # f <- .jfields("com.hs.eyesuite.ext.extperimetry.octo900.ifocto.remote.OCTO900")
    f <- .jfields("com.hs.eyesuite.ext.extperimetry.octo900.ifocto.device.OCTO900")

        #
        # check if cName exists as a field in f. If so, set 
        # .Octopus900Env$cName <- class value for cName
        #
    getC <- function(cName) {
        if (length(grep(cName, f)) > 0) 
            assign(cName,
                   #.jfield("com.hs.eyesuite.ext.extperimetry.octo900.ifocto.remote.OCTO900",NULL,cName)
                   .jfield("com.hs.eyesuite.ext.extperimetry.octo900.ifocto.device.OCTO900",NULL,cName), 
                   envir = .Octopus900Env)
    }

        # get the color fields from OCTO900
    getC("STIM_WHITE")
    getC("STIM_BLUE")
    getC("STIM_RED")
    getC("BG_WHITE")
    getC("BG_YELLOW")
    getC("MET_COL_WW")
    getC("MET_COL_BY")
    getC("MET_COL_RW")
    getC("MET_COL_BLUE_WHITE")
    getC("MET_COL_RED_YELLOW")
    getC("MET_COL_WHITE_YELLOW")
    getC("MET_COL_USER")

    assign("MET_COL_BW", .Octopus900Env$MET_COL_BLUE_WHITE,   envir = .Octopus900Env)
    assign("MET_COL_RY", .Octopus900Env$MET_COL_RED_YELLOW,   envir = .Octopus900Env)
    assign("MET_COL_WY", .Octopus900Env$MET_COL_WHITE_YELLOW, envir = .Octopus900Env)
}


###################################################################
# Goldmann target sizes in degrees
###################################################################
GOLDMANN <- c(6.5, 13, 26, 52, 104) / 60


    # uncomment for Tony's big wheel
#mm <- c(0.125,0.25,0.5,1,1.41,2,2.83,4,5.66,8,11.3,16,22.6,32,64,128,256)
#ind <- c(32,28,31,26,30,29,27,24,25,23,21,22,39,38,20,37,36)
#GOLDMANN <- rep(NA,39)
#GOLDMANN[ind] <- (sqrt(mm/pi)*180/pi/149.1954)

#######################################################################
# INPUT: 
#   eyeSuiteJarLocation      = dir name containing EyeSuite Jar files
#   eyeSuiteSettingsLocation = dir name containing EyeSuite settings
#   eye                      = "right" or "left"
#   gazeFeed                 = 0 (none), 1 (single frame), 2 (all frames with *)
#
#   Both input dirs should INCLUDE THE TRAILING SLASH.
#
# @return NULL if succeed
# @return 1 if already initialised
# @return 2 if failed to make ready
#
#######################################################################
octo900.opiInitialize <- function(eyeSuiteJarLocation=NA, eyeSuiteSettingsLocation=NA, eye=NA, gazeFeed=0) {
    if (is.na(eyeSuiteJarLocation))
        stop("You must specify the EyeSuite jar file folder in your call to opiInitialize")
    if (is.na(eyeSuiteSettingsLocation))
        stop("You must specify the EyeSuite settings folder in your call to opiInitialize")
    if (is.na(eye))
        stop("You must specify which eye ('left' or 'right') in your call to opiInitialize")
    if (eye != "left" && eye != "right")
        stop("The eye argument of opiInitialize must be 'left' or 'right'")

    #options("java.parameters"="-Xmx1024m -Xss64m –verbose:class")
    
    hsJars <- c(
                #paste("C:/opencv-249/build/java/", "opencv-249.jar", sep=""),
                paste(eyeSuiteJarLocation, "opencv-249.jar", sep=""),
                paste(eyeSuiteJarLocation, "opi-6.0.2.0.jar", sep=""),
                paste(eyeSuiteJarLocation, "HSEyeSuiteBasic.jar", sep=""),
                paste(eyeSuiteJarLocation, "HSEyeSuiteExtPerimetry.jar", sep=""),
                paste(eyeSuiteJarLocation, "HSEyeSuiteExtPerimetryViewer.jar", sep=""),
                paste(eyeSuiteJarLocation, "lax.jar", sep=""),
                paste(eyeSuiteJarLocation, "i18n/HSEyeSuiteBasic_i18n-3.1.1.jar", sep=""),
                paste(eyeSuiteJarLocation, "i18n/HSEyeSuitePerimetryExtension_i18n-3.2.1.jar", sep=""),
                paste(eyeSuiteJarLocation, "i18n/HSEyeSuitePerimetryViewer_i18n-3.2.1.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/jgoodies-common-1.4.0.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/jgoodies-binding-2.7.0.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/gettext-commons-0.9.6.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/jcbios.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/jh.jar", sep=""),
                paste(eyeSuiteJarLocation, "jre/lib/ext/mysql-connector-java-5.1.17-bin.jar", sep="")
    )#hsJahrs
    
    .jinit(classpath=hsJars, 
        params=getOption("java.parameters"),
        force.init=TRUE
    )

    print(.jclassPath())    # just for debugging, not really needed

    setupBackgroundConstants()

        # the controling object
    assign("octopusObject", .jnew("opi.Opi", eyeSuiteSettingsLocation, eye), envir = .Octopus900Env)

	err <- .jcall(.Octopus900Env$octopusObject, "I", "opiInitialize", as.double(gazeFeed))
	if (err == 0)
		return(NULL)
	else
		return(err)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#
# Return a list of 
#	err  = string message
#	seen = TRUE if seen, FALSE otherwise
#	time = reaction time
###########################################################################
octo900.opiPresent <- function(stim, nextStim=NULL) { UseMethod("octo900.opiPresent") }
setGeneric("octo900.opiPresent")

octo900.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) {
        stimObj <- .jnull("opi/OpiStaticStimulus")
        nextObj <- .jnull("opi/OpiStaticStimulus")
    } else {
        stimObj <- .jnew("opi/OpiStaticStimulus", stim$x*10.0, stim$y*10.0, cdTodb(stim$level)*10.0)
	    .jcall(stimObj, "V", "setSize", as.double(which.min(abs(GOLDMANN - stim$size))))
	    .jcall(stimObj, "V", "setDuration", as.double(stim$duration))
	    .jcall(stimObj, "V", "setResponseWindow", as.double(stim$responseWindow))
        if (is.null(nextStim)) {
            nextObj <- .jnull("opi/OpiStaticStimulus")
        } else {
            nextObj <- .jnew("opi/OpiStaticStimulus", nextStim$x*10.0, nextStim$y*10.0, 0) # level no matter
	        .jcall(nextObj, "V", "setSize", as.double(which.min(abs(GOLDMANN - nextStim$size))))
	        .jcall(nextObj, "V", "setDuration", as.double(nextStim$duration))
	        .jcall(nextObj, "V", "setResponseWindow", as.double(nextStim$responseWindow))
        }
    }

    if(min(abs(GOLDMANN - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    done <- FALSE
    while (!done) {
	done <- TRUE
    	tryCatch(ret <- .jcall(.Octopus900Env$octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj, nextObj), 
	             java.util.ConcurrentModificationException = function(e) { done = FALSE })
    }

    return(list(
	    err =.jcall(ret, "S", "getErr"), 
	    seen=ifelse(.jcall(ret, "I", "getSeen") == 0, FALSE, TRUE),
	    time=.jcall(ret, "I", "getTime"),
	    frames=NA,#.jcall(ret, "[I", "getFrameInt"),
        numFrames=.jcall(ret, "I", "getNumFrames"),
        width=.jcall(ret, "I", "getWidth"),
        height=.jcall(ret, "I", "getHeight")
	))
}


###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#
# Return a list of 
#	err  = string message
#	seen = TRUE if seen, FALSE otherwise
#	time = reaction time
###########################################################################
octo900.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    if (is.null(stim)) {
        stimObj <- .jnull("opi/OpiTemporalStimulus")
        nextObj <- .jnull("opi/OpiTemporalStimulus")
    } else {
        stimObj <- .jnew("opi/OpiTemporalStimulus", stim$x*10.0, stim$y*10.0, stim$rate)
	    .jcall(stimObj, "V", "setSize", as.double(which.min(abs(GOLDMANN - stim$size))))
	    .jcall(stimObj, "V", "setDuration", as.double(stim$duration))
	    .jcall(stimObj, "V", "setResponseWindow", as.double(stim$responseWindow))
        if (is.null(nextStim)) {
            nextObj <- .jnull("opi/OpiTemporalStimulus")
        } else {
            nextObj <- .jnew("opi/OpiTemporalStimulus", nextStim$x*10.0, nextStim$y*10.0, 10.0) # rate no matter
	        .jcall(nextObj, "V", "setSize", as.double(which.min(abs(GOLDMANN - nextStim$size))))
	        .jcall(nextObj, "V", "setDuration", as.double(nextStim$duration))
	        .jcall(nextObj, "V", "setResponseWindow", as.double(nextStim$responseWindow))
        }
    }

    done <- FALSE
    while (!done) {
	done <- TRUE
    	tryCatch(ret <- .jcall(.Octopus900Env$octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj, nextObj), 
	             java.util.ConcurrentModificationException = function(e) { done = FALSE })
    }

    return(list(
	    err =.jcall(ret, "S", "getErr"), 
	    seen=ifelse(.jcall(ret, "I", "getSeen") == 0, FALSE, TRUE),
	    time=.jcall(ret, "I", "getTime")
	))

}#opiPresent.opiTemporalStimulus()

########################################## TO DO
#	seen = TRUE if seen, FALSE otherwise
octo900.opiPresent.opiKineticStimulus <- function(stim, ...) {
    if (is.null(stim)) {
        stimObj <- .jnull("opi/OpiTemporalStimulus")
    } else { 
            # convert sizes to GOLDMANN
        stim$sizes <- sapply(stim$sizes, function(s) {
            i <- which.min(abs(GOLDMANN - s))
            if(abs(GOLDMANN[i] - s) > 0.000001) {
                warning("opiPresent: Rounding stimulus size to nearest Goldmann size")
            } 
            return(i)
        })

            # bit of a kludge as passing vector of one double seemed to barf
        if (length(stim$path$x) == 2)
            stimObj <- .jnew("opi/OpiKineticStimulus", 
                sapply(stim$path$x, as.double), 
                sapply(stim$path$y, as.double), 
                as.double(cdTodb(stim$levels[1])),
                as.double(stim$sizes[1]),
                as.double(stim$speeds[1]))
        else
            stimObj <- .jnew("opi/OpiKineticStimulus", 
                sapply(stim$path$x, as.double), 
                sapply(stim$path$y, as.double), 
                as.vector(sapply(sapply(stim$levels, cdTodb), as.double)),
                as.vector(sapply(stim$sizes, as.double)), 
                as.vector(sapply(stim$speeds, as.double)))
    }

    done <- FALSE
    while (!done) {
	done <- TRUE
    	tryCatch(ret <- .jcall(.Octopus900Env$octopusObject, "Lopi/OpiPresentReturn;", "opiPresent", stimObj), 
	             java.util.ConcurrentModificationException = function(e) { done = FALSE })
    }

    return(list(
	    err =.jcall(ret, "S", "getErr"), 
	    seen=ifelse(.jcall(ret, "I", "getSeen") == 0, FALSE, TRUE),
	    time=.jcall(ret, "I", "getTime")
	))

}

###########################################################################
#
# Input paras are the Octopus900Env$* constants
# lum is in cd/m^2 (as per OPI spec) * 100 == .Octopus900Env$BG_{OFF | 1 | 10 | 100 }
# color is .Octopus900Env$MET_COL_{WW | BY | RW | BLUE_WHITE | RED_YELLOW | WHITE_YELLOW }
# fixation is .Octopus900Env$FIX_{RING | CROSS | CENTRE}
# fixIntensity is 0..100 %
#
# @return NULL is succeed.
# @return -1 if opiInitialize has not been successfully called
# @return -2 trouble setting backgound color
# @return -3 trouble setting fixation
###########################################################################
octo900.opiSetBackground <- function(lum=NA, color=NA, fixation=NA, fixIntensity=50) {
    ret <- 0
    if (!is.na(color)) {
        if (!is.na(lum))
            ret <- .jcall(.Octopus900Env$octopusObject, "I", "opiSetBackground", as.double(color), as.double(lum*100.0))
        else
            ret <- .jcall(.Octopus900Env$octopusObject, "I", "opiSetBackground", as.double(color))
    }

    if (ret != 0)
        return(ret)

    if (!is.na(fixation))
        ret <- .jcall(.Octopus900Env$octopusObject, "I", "opiSetFixation", as.double(fixation), as.double(fixIntensity))

    if (ret == 0) {
        return(NULL)
    } else {
        return(ret)
    }
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
octo900.opiClose <- function() {
    ret <- .jcall(.Octopus900Env$octopusObject, "I", "opiClose")
    return(NULL)
}

###########################################################################
# Call opiPresent with a NULL stimulus
###########################################################################
octo900.opiQueryDevice <- function() {
    ret <- octo900.opiPresent.opiStaticStimulus(NULL, NULL)
    return(ret$err)
}

#
# Routines to display a point on a plot during simulations.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: August 2012
#
# Copyright [2022] [Andrew Turpin]
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

library(grDevices)

# Globals
# .OpiEnv$SimDisplay$display    # device number of plot, or NA
# .OpiEnv$SimDisplay$bg         # bg color for plot
# .OpiEnv$SimDisplay$grid.color # color of display grid or NA

if (exists(".OpiEnv") && !exists("SimDisplay", where=.OpiEnv))
    assign("SimDisplay", new.env(3), envir=.OpiEnv)

###########################################################################
# dimensions = c(xlo,xhi,ylo,yhi)
# Return
#   0 success or display is NULL
#   1 fail
###########################################################################
simDisplay.setupDisplay <- function(dimensions = NA) {
    assign("display", NA, envir = .OpiEnv$SimDisplay)
    if (is.na(dimensions))       return(0)
    if (length(dimensions) != 4) return(1)

    plot(0,0,type="n", 
        xlim=range(dimensions[1:2]), 
        ylim=range(dimensions[3:4]), 
        xlab="",
        ylab=""
    )
    assign("display"   , dev.cur(), envir = .OpiEnv$SimDisplay)
    assign("bg"        , "white",   envir = .OpiEnv$SimDisplay)
    assign("grid.color", grey(0.5), envir = .OpiEnv$SimDisplay)
    grid(lty=2, col=.OpiEnv$SimDisplay$grid.color)

    return (0)
}

#######################################################################################
# col = background color of plot
# gridCol = color of grid
# Return
#   NULL - success
#     -1 -  setupDisplay has not been called
#
simDisplay.setBackground <- function(col, gridCol) { 
    if(!exists("display", envir=.OpiEnv$SimDisplay))
        return(-1)

    if (is.na(.OpiEnv$SimDisplay$display))
        return(NULL)

    if(is.na(col)) {
        assign("bg" , "white", envir = .OpiEnv$SimDisplay)
    } else {
        assign("bg" , col    , envir = .OpiEnv$SimDisplay)
    }
    assign("grid.color", gridCol, envir = .OpiEnv$SimDisplay)

    if (dev.cur() != .OpiEnv$SimDisplay$display) {  # check if window was closed
        assign("display", NA, envir = .OpiEnv$SimDisplay)
    } else {
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = .OpiEnv$SimDisplay$bg)
        if (!is.na(gridCol)) 
            grid(lty=2, col=gridCol)
    }

    return(NULL)
}

##################################################################################
# Show stim on plot for duration and wait responseWindow after.
# No return, just die quietly if neccessary.
#
simDisplay.present <- function(x, y, color, duration, responseWindow) {
    if(exists("display", envir=.OpiEnv$SimDisplay)) {
        if (!is.na(.OpiEnv$SimDisplay$display)) {
            if (dev.cur() != .OpiEnv$SimDisplay$display) {  # check if window was closed
                assign("display", NA, envir = .OpiEnv$SimDisplay)
            } else {
                startTime <- Sys.time()
                points(x, y, pch=19, col=color)
                dt <- difftime(Sys.time(), startTime, units="secs")*1000
                while (dt < duration)
                    dt <- difftime(Sys.time(), startTime, units="secs")*1000
                points(x, y, pch=19, col=.OpiEnv$SimDisplay$bg)  # blank it
                while (dt < duration + responseWindow)
                    dt <- difftime(Sys.time(), startTime, units="secs")*1000
            }
        }
    }
}

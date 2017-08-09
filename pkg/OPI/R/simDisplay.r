#
# Routines to display a point on a plot during simulations.
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

# Globals
# .SimDisplayEnv$display    # device number of plot, or NA
# .SimDisplayEnv$bg         # bg color for plot
# .SimDisplayEnv$grid.color # color of display grid or NA
# .SimDisplayEnv$dimensions # last used dimensions of the plot, or NA

.SimDisplayEnv <- new.env(size=4)

###########################################################################
# dimensions = c(xlo,xhi,ylo,yhi)
# Return
#   0 success or display is NULL
#   1 fail
###########################################################################
simDisplay.setupDisplay <- function(dimensions, bg="white", gridCol=grey(0.5)) {
    
    assign("display"   , NA, envir = .SimDisplayEnv)
    assign("bg"        , NA, envir = .SimDisplayEnv)
    assign("grid.color", NA, envir = .SimDisplayEnv)
    assign("dimensions", NA, envir = .SimDisplayEnv)
    
    if (is.null(dimensions))     return(0)
    if (length(dimensions) != 4) return(1)
  
    return (simDisplay.resetDisplay(dimensions = dimensions, backgroundCol = bg, gridCol = gridCol));
    
}

#######################################################################################
# col = background color of plot
# gridCol = color of grid
# Return
#   NULL - success
#     -1 -  setupDisplay has not been called
#######################################################################################
simDisplay.setBackground <- function(col, gridCol) { 
    if(!exists("display", envir=.SimDisplayEnv))
        return(-1)

    tCol <- if(is.na(col)) "white" else col
    assign("bg", tCol , envir = .SimDisplayEnv)
  
    tGridCol <- if(is.na(gridCol)) grey(0.5) else gridCol
    assign("grid.color", tGridCol, envir = .SimDisplayEnv)
  
    if (!is.na(.SimDisplayEnv$display)) {
        if (dev.cur() != .SimDisplayEnv$display) {  # check if window was closed
            assign("display", NA, envir = .SimDisplayEnv)
        } else {
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = .SimDisplayEnv$bg)
            if (!is.na(gridCol)) 
                grid(lty=2, col=gridCol)
        }
    }
    return(NULL)
}

##################################################################################
# Show stim on plot for duration and wait responseWindow after.
# No return, just die quietly if neccessary.
##################################################################################
simDisplay.present <- function(x, y, color, duration, responseWindow, size = 0.43) {
    if(exists("display", envir=.SimDisplayEnv)) {
        if (!is.na(.SimDisplayEnv$display)) {
            if (dev.cur() != .SimDisplayEnv$display) {  # check if window was closed
                assign("display", NA, envir = .SimDisplayEnv)
            } else {
                
                simDraw.circle(x, y, size/2, col = color)
                Sys.sleep(duration/1000.0)
                
                simDisplay.resetDisplay()
                Sys.sleep(abs(responseWindow - duration)/1000.0)
            }
        }
    }
}

###########################################################################
# dimensions = c(xlo,xhi,ylo,yhi)
# backgroundCol = background color of the plot
# gridCol = grid color of the plot
# Return
#   0 success or display is NULL
#   1 fail
###########################################################################
simDisplay.resetDisplay <- function(dimensions = NULL, backgroundCol=NA, gridCol=NA) {

  # Load display parameters.
  dimm <- if(is.null(dimensions)) .SimDisplayEnv$dimensions else dimensions
  if (length(dimm) != 4)  return(1)
  
  bg <- if(!is.na(backgroundCol)) backgroundCol else 
    if(!is.na(.SimDisplayEnv$bg)) .SimDisplayEnv$bg else "white"
  
  gridColor <- if(!is.na(gridCol)) gridCol else
    if(!is.na(.SimDisplayEnv$grid.color)) .SimDisplayEnv$grid.color else grey(0.5)
  
  if(!exists("display", envir=.SimDisplayEnv) | is.na(.SimDisplayEnv$display)) {
    assign("display", dev.cur(), envir = .SimDisplayEnv)
  }
  
  assign("dimensions", dimm     , envir = .SimDisplayEnv)
  assign("bg"        , bg       , envir = .SimDisplayEnv)
  assign("grid.color", gridColor, envir = .SimDisplayEnv)
  
  # Empty Plot
  simDraw.emptyPlot(dimm, gridColor = gridColor)
  
  # Set Background
  if(bg != "white") {
    simDisplay.setBackground(col= bg, gridCol = gridColor)
  }
  
  return(0)
}

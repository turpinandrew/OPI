#
# Routines to help the simulator to display a stimulus on a plot during simulations.
#
# Author: Danish Amjad    (danishamjad30@gmail.com)
# Date: August 2017
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
simDraw.emptyPlot <- function(dimensions, gridColor = grey(0.5)) {
  plot(0,0,type="n", 
       xlim=range(dimensions[1:2]), 
       ylim=range(dimensions[3:4]), 
       xlab="",
       ylab=""
  )
  grid(lty=2, col=gridColor)
}

simDraw.plotMarker <- function (x, y, radius, sides = 3, angle = 0, border = NULL, col = NA, lty = 1, lwd = 1) 
{
  for (circle in 1:length(radius)) {
    points <-  simDraw.getShape(x = x, y = y, radius = radius, sides = sides, angle = angle, border = border, col = col, lty = lty, lwd = lwd)
    polygon(points$x, points$y, border = border, col = col[circle], 
            lty = lty, lwd = lwd)
  }
  invisible(list(x = points$x, y = points$y))
}

simDraw.getShape <- function (x, y, radius, sides = 3, angle = 0, border = NULL, col = NA, lty = 1, lwd = 1) 
{
  xylim <- par("usr")
  angle.inc <- 2 * pi/sides
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  if (length(col) < length(radius)) 
    col <- rep(col, length.out = length(radius))
  for (shape in 1:length(radius)) {
    xv <- cos(angles + angle) * radius[shape] + x
    yv <- sin(angles + angle) * radius[shape] + y
  }
  invisible(list(x = xv, y = yv))
}

simDraw.circle <- function (x, y, radius, border = NULL, col = NA, lty = 1, lwd = 1) {
  return (simDraw.plotMarker(x, y, radius, sides = 100, border = border, col = col, lty = lty, lwd = lwd))
}

simDraw.square <- function (x, y, side, border = NULL, col = NA, lty = 1, lwd = 1) {
  radius <- side / sqrt(2)
  return (simDraw.plotMarker(x, y, radius, angle = pi/4, sides = 4, border = border, col = col, lty = lty, lwd = lwd))
}

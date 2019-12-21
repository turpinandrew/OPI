# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
#
# Modified Tue  8 Jul 2014: make generic for any dB scale
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

#' @rdname dbTocd
#' @title Convert dB to cd/\eqn{\mbox{m}^2}{m^2}
#' @description Given a value in dB, return the cd/\eqn{\mbox{m}^2}{m^2}
#' equivalent. Default is to use HFA units, so maximum stimulus is 10000
#' apostilbs
#' @param db Value to convert to cd
#' @param maxStim Stimulus value for 0dB in cd/\eqn{\mbox{m}^2}{m^2}
#' @return \code{dbTocd} returns cd/\eqn{\mbox{m}^2}{m^2} value
#' @examples
#' # decibels to candela
#' cd <- dbTocd(0)   # 10000/pi
#' cd <- dbTocd(10)  # 1000/pi
#' cd <- dbTocd(20)  # 100/pi
#' cd <- dbTocd(30)  # 10/pi
#' cd <- dbTocd(40)  # 1/pi
#' @export
dbTocd <- function(db, maxStim=10000/pi) { maxStim * 10^(-db/10) }

#' @rdname dbTocd
#' @param cd Value to convert to dB in cd/\eqn{\mbox{m}^2}{m^2}
#' @return \code{cdTodb} returns a dB value.
#' @examples
#' # candela to decibels
#' dB <- cdTodb(10000/pi)  # 0 dB
#' dB <- cdTodb(1000/pi)   # 10 dB
#' dB <- cdTodb(100/pi)    # 20 dB
#' dB <- cdTodb(10/pi)     # 30 dB
#' dB <- cdTodb(1/pi)      # 40 dB
#' dB <- cdTodb(0.1/pi)    # 50 dB
#' @export
cdTodb <- function(cd, maxStim=10000/pi) { 
    stopifnot(cd > 0)
    -10*log10(cd/maxStim) 
}

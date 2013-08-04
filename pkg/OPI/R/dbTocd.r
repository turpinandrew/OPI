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

    # Convert HFA dB to cd/m^2
dbTocd <- function(db) {
    asb <- 10^((40-db)/10) 
    return (asb/pi)
}

    # Convert cd/m^2 to HFA dB
cdTodb <- function(cd) {
    asb <- cd * pi
    return (40 - 10*log10(asb))
}

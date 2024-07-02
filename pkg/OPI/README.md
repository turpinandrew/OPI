# Open Perimetry Interface (OPI)

The OPI package (Open Perimetry Interface) is used to both simulate and execute visual field testing 
algorithms (perimetry algorithms) on machines that can create visual stimuli. 
The simulation can be done solely with this package. Controlling machines, be they standard 
displays, phones, tablets, etc or commercial perimeters 
requires the OPI Monitor Java package. Use of commercial machines usually requires permission 
from the manufacturer and some other relevant third party software or key.

## Why should I use it?

You should use this package for implementing, developing, testing and using visual 
psychophysics test procedures particularly (but not limited to) those used for 
clinical visual field testing (perimetry).

## How do I use it?

The package supports 5 R functions that work across multiple platforms: 
 * `opiInitialize()` For creating a connection to a machine or simulated subject.
 * `opiSetup()` For setting parameters like fixation markers, background colors, luminance, etc.
 * `opiPresent()` For presenting visual stimuli and getting a response.
 * `opiClose()` For closing a connection with a machine or simulated subject.
 * `opiQuery()` For getting information about a machine or simulated subject.

The machine or simulation that the set of functions operate upon is selected using the `chooseOPI()` function.

## How do I get it?

The OPI package is free from CRAN and includes all code needed for simulation and use on 
machines such as standard displays (ie your computer monitor). 

To use the 
 * Octopus 900 machine, you should contact Haag-Streit to seek permission.
 * imoVifa/Tempo machine, contact CREWT (Japan)or Topcon Healthcare Inc (USA) to seek permission.
 * Compass or Maia machine, contact iCare to seek permission.
 * Kowa AP7000 machine, contact Kowa to seek permission.

Note the reason for requiring permission from commercial partners of the OPI
for use on their machines is for medico-legal reasons related to the 
accreditation of the machines as medical devices by regulating bodies in 
different countries.  

If you do not have contact details with the companies, email 
[Andrew Turpin](mailto:andrew.turpin@lei.org.au)
and he can put you in touch with the right person.

## Other resources

[OPI www page](https://opi.lei.org.au)

[OPI github](https://github.com/turpinandrew/OPI)

[OPI Discourse Forum](https://openperimetry.org/)
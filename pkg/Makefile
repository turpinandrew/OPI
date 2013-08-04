	# R files for OPI package
OPI_R= dbTocd.r full_threshold.r mocs.r multiLocation.r octopus900.r opi.r simG.r simH.r zest.r simDisplay.r simH_RT.r

	# Rd files for OPI package
OPI_MAN= FT.Rd OPI-package.Rd ZEST.Rd cdTodb.Rd dbTocd.Rd opiClose.Rd opiInitialize.Rd opiKineticStimulus.Rd opiPresent.Rd opiQueryDevice.Rd opiSetBackground.Rd opiStaticStimulus.Rd opiTemporalStimulus.Rd RtSigmaUnits.Rd RtDbUnits.Rd

OPI_ROOT= OPI/DESCRIPTION OPI/LICENCE OPI/NAMESPACE OPI/GPL3

OPI_FILES= $(OPI_ROOT) $(addprefix OPI/man/,$(OPI_MAN)) $(addprefix OPI/R/,$(OPI_R))


build: 
	R CMD build OPI

install:
	R CMD install OPI_1.6.tar.gz

check: OPI.Rcheck 

clean:
	\rm -rf OPI.Rcheck OPIOctopus900.Rcheck OPIOctopus900/inst/java/api

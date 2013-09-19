# Makefile to build OPI

OPI_VERSION := $(shell cat OPI/DESCRIPTION | grep Version | sed 's/Version: //')



# R files for OPI package
OPI_R			=	dbTocd.r \
						full_threshold.r \
						full_threshold_state.r \
						mocs.r \
						multiLocation.r \
						octopus900.r \
						opi.r \
						simG.r \
						simH.r \
						zest.r \
						simDisplay.r

# Rd files for OPI package
OPI_MAN		=	FT.Rd \
						OPI-package.Rd \
						ZEST.Rd \
						cdTodb.Rd \
						dbTocd.Rd \
						opiClose.Rd \
						opiInitialize.Rd \
						opiKineticStimulus.Rd \
						opiPresent.Rd \
						opiQueryDevice.Rd \
						opiSetBackground.Rd \
						opiStaticStimulus.Rd \
						opiTemporalStimulus.Rd

OPI_ROOT	=	DESCRIPTION \
						LICENCE \
						NAMESPACE \
						GPL3

OPI_FILES	=	$(addprefix OPI/,$(OPI_ROOT)) \
						$(addprefix OPI/man/,$(OPI_MAN)) \
						$(addprefix OPI/R/,$(OPI_R))



all: check build install

check: OPI.Rcheck

clean:
	rm -rf OPI*.Rcheck
	rm -rf OPI*.tar.gz

build: 
	R CMD build OPI

install:
	R CMD install OPI_$(OPI_VERSION).tar.gz

OPI.Rcheck: $(OPI_FILES)
	R CMD check OPI


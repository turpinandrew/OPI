# Makefile to build OPI

OPI_VERSION := $(shell cat OPI/DESCRIPTION | grep Version | sed 's/Version: //')

# R files for OPI package
OPI_R =	\
    dbTocd.r \
    fourTwo.r full_threshold.r zest.r mocs.r \
    imoClient.r kowaAP7000Client.r octopus600.r octopus900Client.r \
    opi.r \
    simDisplay.r simG.r simH.r simH_RT.r simNo.r simYes.r

# Rd files for OPI package
OPI_MAN	= \
    FT.Rd RtDbUnits.Rd RtSigmaUnits.Rd ZEST.Rd cdTodb.Rd\
    chooseOpi.Rd dbTocd.Rd fourTwo.Rd opiClose.Rd opiInitialize.Rd opiKineticStimulus.Rd\
    opiPresent.Rd opiQueryDevice.Rd opiSetBackground.Rd opiStaticStimulus.Rd\
    opiTemporalStimulus.Rd

OPI_ROOT	=	DESCRIPTION \
				NAMESPACE

OPI_FILES	=	$(addprefix OPI/,$(OPI_ROOT)) \
				$(addprefix OPI/man/,$(OPI_MAN)) \
				$(addprefix OPI/R/,$(OPI_R))


all: build check install

check: OPI.Rcheck

clean:
	rm -rf OPI*.Rcheck
	rm -rf OPI*.tar.gz

build: 
	(cd OPI ; R -e 'library(devtools);document()')
	R CMD build OPI

install:
	R CMD INSTALL OPI_$(OPI_VERSION).tar.gz

OPI.Rcheck: $(OPI_FILES)
	(cd OPI ; R -e 'library(devtools);document()')
	R CMD check --as-cran OPI_$(OPI_VERSION).tar.gz
	#R CMD check OPI


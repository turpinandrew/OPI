# Makefile to build OPI

OPI_VERSION := $(shell cat OPI/DESCRIPTION | grep Version | sed 's/Version: //')

# R files for OPI package
OPI_R =	\
    Compass.r Display.r ImoVifa.r KTPsi.r KowaAP7000Client.r OPI-package.r Octopus600.r \
    Octopus900.r PhoneHMD.r PicoVR.r QUESTP.r SimGaussian.r SimHenson.r SimHensonRT.r SimNo.r \
    SimYes.r data-RtDbUnits.r data-RtSigmaUnits.r dbTocd.r fourTwo.r full_threshold.r mocs.r \
    opi.r pix2deg.r zest.r

OPI_ROOT	=	DESCRIPTION \
				NAMESPACE

OPI_FILES	=	$(addprefix OPI/,$(OPI_ROOT)) \
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


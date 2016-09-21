library('RUnit')

setwd("/Users/astrid/Documents/R/OPI/pkg/OPI/R")
source('simH_RT.R')

test.suite <- defineTestSuite("Kinetic stim in HensonRT",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)




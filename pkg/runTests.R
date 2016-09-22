library('RUnit')

test.suite <- defineTestSuite("Kinetic stim in HensonRT",
                              dirs = file.path("./OPI/tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)




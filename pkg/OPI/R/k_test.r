
# install.package("OPI_2.2.tar.gz", NULL, type="source")

### require(OPI)
### 
### chooseOpi("KowaAP7000")
### 
### res <- opiInitialise()
### if (!is.null(res))
###     stop('opiInitialise failed')
### 
### makeStim <- function(db) { 
###      s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=26/60,
###                 color=.KowaAP7000Env$COLOR_WHITE,
###                 duration=200, responseWindow=1500)
###      class(s) <- "opiStaticStimulus"
###      
###      return(s)
### }
###      
### #res <- opiPresent(stim=makeStim(10), nextStim=NULL)
### #print(res)
### 
### res <- opiClose()
### if (!is.null(res)) {
###     print("opiClose() failed")
### } else {
###     print("opiClose() succeeded")
### }

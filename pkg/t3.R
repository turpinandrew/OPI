require(OPI)
chooseOpi("Display")

opiInitialise()

text(0,0, "Press a key when resized to your liking")
getGraphicsEvent("Waiting for key press", onKeybd = function(key) return(TRUE))

opiSetBackground(color=grey(0.8), fix_type=.OpiEnv$Display$FIX_CROSS)

s <- list(x=10, y=10, size=0.43, color="red", duration=200, responseWindow=1500)
class(s) <- "opiStaticStimulus"

resps <- sapply(1:10, function(i) {Sys.sleep(runif(1));opiPresent(s)})

#
#opiSetBackground(color="green", fix_type=.OpiEnv$Display$FIX_CROSS, fix_sx=10, fix_color="purple")
#resps <- sapply(1:1, function(i) {Sys.sleep(runif(1));opiPresent(s)})
#
opiClose()
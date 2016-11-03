#
# Check FP=100% and FN=100%
#

test.false_positive <- function() { 
    r <- check_falsies(1,0) 
    checkTrue(r, "fpr set to 1 so stim should be seen")
}

test.false_negative <- function() { 
    r <- check_falsies(0,1) 
    checkTrue(!r, "fnr set to 1 so stim should not be seen")
}



################################################
# Given the two false repsonse rates, 
# return seen/not-seen
################################################
check_falsies <- function(fpr=0,fnr=0)
{
  require(OPI)
  data("RtDbUnits")
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')

  e <- opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=RtDbUnits, rtFP=1:1600)
  checkEquals(NULL, e)
  
  SPEED = 3;

  LEVEL = dbTocd(0,10000)

  stim <- list(path=list(x=c(20,10), y=c(-5,32)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
  class(stim) <- "opiKineticStimulus"

  result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), fpr=fpr, fnr=fnr)

  checkEquals(NULL, opiClose())

  return(result$seen)
}

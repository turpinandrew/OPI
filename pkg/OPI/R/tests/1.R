test.exampleVectors <- function()
{
  require(OPI)
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')
  checkEquals(NULL, simH_RT.opiClose())
  
  stim <- list(path=list(x=c(3,2), y=c(0,-70)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
  class(stim) <- "opiKineticStimulus"
  
  opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=RtDbUnits, rtFP=1:1600)
  result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), #list(seq(0, 30, length=10)),
                       fpr=0, fnr=0)
  
  checkTrue(result$seen)

  if (stim$path[1]$x[1]>stim$path[1]$x[2]){
    checkTrue(result$x <= stim$path[1]$x[1])
    checkTrue(result$x >= stim$path[1]$x[2])
  }
  else {
    checkTrue(result$x >= stim$path[1]$x[1])
    checkTrue(result$x <= stim$path[1]$x[2])
  }
  
  if (stim$path[2]$y[1]>stim$path[2]$y[2]){
    checkTrue(result$y <= stim$path[2]$y[1])
    checkTrue(result$y >= stim$path[2]$y[2])
  }
  else {
    checkTrue(result$y >= stim$path[2]$y[1])
    checkTrue(result$y <= stim$path[2]$y[2])
  }
#  checkEqualsNumeric(6, factorial(3))
#  checkIdentical(6, factorial(3))
#  checkTrue(2 + 2 == 4, 'Arithmetic works')
  #checkEquals(6, factorial(3))
  #checkException(log('a'), 'Unable to take the log() of a string')

}
# 
# test.deactivation <- function()
# {
#   DEACTIVATED('Deactivating this test function')
# }
# 

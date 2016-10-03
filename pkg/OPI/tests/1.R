require(RUnit)

test.exampleVectors <- function()
{
  require(OPI)
  data("RtDbUnits")
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')
  checkEquals(NULL, opiClose())
  
  #xs <- list([3,2], [-20,-20])
  xs <- c(3, 2, -20, 0, 10,5, -5, -50, 3, 2, -20, 0, 10,5, -5, -50) #, 
  x_mat <- matrix(xs, 2)
  ys <- c(0, -70, -20, 0, 14,2, -23, -30, 3, 2, -20, 0, 10,5, -5, -50) #0, -70,
  y_mat <- matrix(ys, 2)
  pointsTested = length(y_mat[1,])
  
  SPEED = 3;
  LEVEL = dbTocd(0,10000)
  
  for (i in 1:pointsTested) {
    stim <- list(path=list(x=x_mat[,i], y=y_mat[,i]),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
    class(stim) <- "opiKineticStimulus"
    
    opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=RtDbUnits, rtFP=1:1600)
    result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), #list(seq(0, 30, length=10)),
                         fpr=0, fnr=0)
    
    checkTrue(result$seen)
  
    if (stim$path[1]$x[1]>stim$path[1]$x[2]){
      checkTrue(result$x <= stim$path[1]$x[1])
      checkTrue(result$x >= stim$path[1]$x[2], paste0("result$x should be greater than path. result$x = ", result$x, " stim$path[1]$x[2] = ",stim$path[1]$x[2]) )#, " path val = ", stim$path[1]$x[2]) )
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
  }
  
  #check for an unseen stimulus (below threshold)
  LEVEL = dbTocd(50,10000)
  for (i in 1:pointsTested) {
    stim <- list(path=list(x=x_mat[,i], y=y_mat[,i]),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
    class(stim) <- "opiKineticStimulus"
    
    opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=RtDbUnits, rtFP=1:1600)
    result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), #list(seq(0, 30, length=10)),
                         fpr=0, fnr=0)
    checkTrue(!result$seen)
  }
  
  #check for fpr
  result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), #list(seq(0, 30, length=10)),
                       fpr=1, fnr=0)
  checkTrue(result$seen, "fpr set to 1 so stim should be seen")

  # #check for fnr
  LEVEL = dbTocd(0,10000)
  stim <- list(path=list(x=c(20,10), y=c(-5,32)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
  class(stim) <- "opiKineticStimulus"
  result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), #list(seq(0, 30, length=10)),
                       fpr=0, fnr=1)
  checkTrue(!result$seen, "fnr set to 1 so stim should not be seen")
}
#  checkEqualsNumeric(6, factorial(3))
#  checkIdentical(6, factorial(3))
#  checkTrue(2 + 2 == 4, 'Arithmetic works')
  #checkEquals(6, factorial(3))
  #checkException(log('a'), 'Unable to take the log() of a string')

# 
# test.deactivation <- function()
# {
#   DEACTIVATED('Deactivating this test function')
# }
# 

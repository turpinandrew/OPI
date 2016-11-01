#require(RUnit)

  #check for an unseen stimulus (below threshold)

test.exampleVectors <- function()
{
  require(OPI)
  data("RtDbUnits")
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')
  checkEquals(NULL, opiClose())
  
  xs <- c(3, 2, -20, 0, 10,5, -5, -50, -20, 0, 10,5, -5, -50) #, 3, 2,
  x_mat <- matrix(xs, 2)
  ys <- c(0, -70, -20, 0, 14,2, -23, -30, -20, 0, 10,5, -5, -50) #3, 2,
  y_mat <- matrix(ys, 2)
  pointsTested = length(y_mat[1,])

  SPEED = 3;
  LEVEL = dbTocd(50,10000)

  for (i in 1:pointsTested) {
    stim <- list(path=list(x=x_mat[,i], y=y_mat[,i]),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
    class(stim) <- "opiKineticStimulus"
    
    opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=RtDbUnits, rtFP=1:1600)

    result <- opiPresent(stim, tt= list(c(0,0,0,0,0)), fpr=0, fnr=0)

    checkTrue(!result$seen)
  }

  opiClose()
}

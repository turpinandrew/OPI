#require(RUnit)

##################################
# Check that RT are consistent
# A vector with rt==0 should have stop before rt=1000
##################################

require(OPI)

get_mean_dist<- function(rtData)
{
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')
  checkEquals(NULL, opiClose())
  
  SPEED = 3;
  LEVEL = dbTocd(20,10000)

  e <- opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData=rtData, rtFP=1:1600)
  checkEquals(NULL, e)

  stim <- list(path=list(x=c(20,0), y=c(20,0)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
  class(stim) <- "opiKineticStimulus"

  result <- opiPresent(stim, tt= list(c(0,40)), fpr=0, fnr=0)

  checkEquals(NULL, opiClose())

  return(sqrt((result$x - 20)^2 + (result$y-20)^2))
}


test.rt <- function()
{
  
  rtData1 <- as.data.frame(cbind(Rt=rep(   0, 100),Dist=rep(0, 100), Person=rep(0, 100)))
  rtData2 <- as.data.frame(cbind(Rt=rep(1000, 100),Dist=rep(0, 100), Person=rep(0, 100)))

  d1 <- median(replicate(100, get_mean_dist(rtData1)))
  d2 <- median(replicate(100, get_mean_dist(rtData2)))

  checkTrue(d1 < d2, "d1 (rt=0) should be less than d2 (rt=1000)")
}

##################################
# Check that Speeds are consistent
# A vector with low speed should have stop before vetor with hi speed, cet par.
##################################

require(OPI)
chooseOpi("SimHensonRT")

rtData <- as.data.frame(cbind(Rt=rep(0, 100),Dist=rep(0, 100), Person=rep(0, 100)))

e <- opiInitialize(type="C", A=NA, B=NA, cap=6, display=NA, maxStim=10000/pi, rtData=rtData, rtFP=1:1600)

get_mean_dist<- function(speed)
{
  LEVEL = dbTocd(20,10000)

  stim <- list(path=list(x=c(20,0), y=c(20,0)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(speed))
  class(stim) <- "opiKineticStimulus"

  result <- opiPresent(stim, tt= list(c(0,40)), fpr=0, fnr=0)

  return(sqrt((result$x - 20)^2 + (result$y-20)^2))
}


test.speed <- function()
{
  
  d1 <- median(replicate(100, get_mean_dist(1)))
  d5 <- median(replicate(100, get_mean_dist(5)))

  checkTrue(d1 < d5, "d1 (speed 1) should be less than d5 (speed 5)")
}

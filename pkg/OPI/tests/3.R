#require(RUnit)

  #check for fp

test.exampleVectors <- function()
{
  require(OPI)
  data("RtDbUnits")
  checkTrue(chooseOpi("SimHensonRT"), 'RT Henson sim is chosen')
  checkEquals(NULL, opiClose())
  
  SPEED = 3;
  LEVEL = dbTocd(50,10000)

  stim <- list(path=list(x=c(20,10), y=c(-5,32)),sizes=(0.43), colors=("white"),  levels=(LEVEL), speeds=c(SPEED))
  class(stim) <- "opiKineticStimulus"

  result <- opiPresent(stim, tt= list(c(30,30,30,30,30)), fpr=1, fnr=0)

  checkTrue(result$seen, "fpr set to 1 so stim should be seen")

}

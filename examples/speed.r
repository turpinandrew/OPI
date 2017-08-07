#
# Measure reaction times to numStims presenations of 
# Size III white-on-white stimuli at random locations 
# in the central 30 degrees of the field.
# Save results in a file.
#
# This was used by me as a demo at my university's Open Day
# August 2012.
#
# Andrew Turpin 17/8/2012 (aturpin@unimelb.edu.au)
#

numStims <- 20  # number per go

# chooseOpi("Octopus900")
# opiInitialize(
#   eyeSuiteJarLocation="C:/Program Files/EyeSuite/",
#   eyeSuiteSettingsLocation="C:/Program Files/EyeSuite/",Mu
#   eye="right"
# )

chooseOpi("SimGaussian")
opiInitialize(sd=2)


##########################################################################
# Helper function to create stim objects
##########################################################################
makeStim <- function(x, y, db, rt) {
  s <- list(x=x, y=y, level=dbTocd(db, 4000/pi), size=26/60, color="white",
            duration=200, responseWindow=rt)
  
  class(s) <- "opiStaticStimulus"
  return(s)
}


##########################################################################
# Read high scores and get name of current user
##########################################################################
# This is the initial, empty file
#"name" "V1" "V2" "V3" "V4" "V5" "V6" "V7" "V8" "V9" "V10" "V11" "V12" "V13" "V14" "V15" "V16" "V17" "V18" "V19" "V20"
#"Dr T" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20"

options(stringsAsFactors = FALSE)
globalScoreTable <- read.table("openDayReactionTimes.csv", colClasses=c("character",rep("numeric",numStims)))
globalScoreTable <- rbind(globalScoreTable, NA)

cat("Enter Name (for high score table!): ")
globalScoreTable[nrow(globalScoreTable),1] <- readline()

##########################################################################
# Initialise matrix of stims and then present
##########################################################################
xy <- matrix(runif(2*numStims,-30,30), ncol=2) # random (x,y) coords
rt <- 1000	# speed 
res <- NULL
for(i in 1:nrow(xy)) {
  s <- makeStim(xy[i,1], xy[i,2], 10, rt)
  if (i == nrow(xy))
    n <- s
  else
    n <- makeStim(xy[i+1,1], xy[i+1,2], 10, rt)
  r <- opiPresent(s,n)
  res <- c(res, list(r))
  
  if (r$seen)
    globalScoreTable[nrow(globalScoreTable),i+1] <- 1000
  else
    globalScoreTable[nrow(globalScoreTable),i+1] <- r$time
}

opiClose()

##########################################################################
# Store the high score
##########################################################################
numberBits <- 2:(numStims+1)

hist(as.numeric(globalScoreTable[nrow(globalScoreTable), numberBits]))

write.table(globalScoreTable, file="openDayReactionTimes.csv", quote=TRUE, row.names=FALSE, col.names=FALSE)

means <- apply(globalScoreTable[,numberBits], 1, mean)

cat("====================================================")
cat("====================================================")
cat("Your time (on average) was ")
cat(means[nrow(globalScoreTable)])
cat(" which is ")
cat(rank(means)[nrow(globalScoreTable)])
cat("/")
cat(nrow(globalScoreTable))
cat(" fastest for the day.\n")

cat("You pressed early ")
cat(sum(unlist(lapply(res, "[", "err")) == "NotValid"))
cat(" times, which incurs max penalty of 1000 ms.\n")

cat("You did not see   ")
cat(sum(unlist(lapply(res, "[", "err")) == "NotSeen"))
cat(" targets, which also incurs the max penalty.\n")


cat("\n\n----------------------\n")
cat("Fastest ten mean times\n")
cat("----------------------\n")
o <- head(order(means), 10)
cat(sprintf("%10.3fms %s\n",means[o], cbind(globalScoreTable[o,1])))
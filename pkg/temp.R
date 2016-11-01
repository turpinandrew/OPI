require(OPI)
#chooseOpi("Octopus900")
chooseOpi("SimHensonRT")
data("RtDbUnits")

EYE <- "right"
gazeFeed  <- 0
opiInitialise(eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/", eye = EYE, 
              gazeFeed = gazeFeed,
              bigWheel = TRUE,
              zero_dB_is_10000_asb = TRUE,
              rtData=RtDbUnits
)
options(error=opiClose)

#LEVELS <- 0:4 * 10
LEVELS <- c(40)
REPS <- 1 # 100
TT_REPS <- 10 # 1000

res <- NULL
for (level in LEVELS) {
    stim <- list(
        path=list(x=c(50, 1), y=c(50,1)),
	sizes=c(0.43), 
        colors=c("white"),  
	levels=c(dbTocd(level)), 
	speeds=c(3))
    class(stim) <- "opiKineticStimulus"


    r <- lapply(TT_REPS, function(len) {
	sapply(1:REPS, function(i) {
		tts <- seq(1,50,length.out=len) 
		x <- opiPresent(stim=stim, tt=list(tts), 
				fpr=0.0, fnr=0, notSeenToSeen=TRUE)$x
        x
		
	  })
    })

    res <- c(res, r)
}

print(res)
#boxplot(res)


#stim <- list(
#        x=3, y=7,
#	      size=c(0.43), 
#        color="white",  
#	      level=dbTocd(0), 
#        duration=200,
#        responseWindow=1500)
#class(stim) <- "opiStaticStimulus"
#
#for (i in 1:10)
#result <- opiPresent(stim=stim, tt=30, fpr=0.0, fnr=0)


opiClose()

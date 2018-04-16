
O900 <- TRUE
EYE <- "right"
ZERO_DB <- 10000

if (!O900) {
  chooseOpi("SimHenson")
  if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")
} else {
  chooseOpi("Octopus900")
  opiInitialize(type="C", cap=6, display=NULL,
                eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
                eye=EYE,
                bigWheel=TRUE,
                pres_buzzer=0, 
                resp_buzzer=3,
                zero_dB_is_10000_asb = (ZERO_DB == 10000),
                gaze_feed=1
  )
  opiSetBackground(fixation=.Octopus900Env$FIX_CENTRE, fixIntensity=100)
}


makeStimHelper <- function(x, y) {  # returns a function of (db,n)
  ff <- function(db, n) db+n
  
  body(ff) <- substitute(
    {s <- list(x=x, y=y, level=dbTocd(db, Z/pi), size=0.43, color="white",
               duration=200, responseWindow=1500, checkFixationOK=NULL)
    class(s) <- "opiStaticStimulus"
    return(s)
    }
    , list(x=x,y=y,Z=ZERO_DB))
  return(ff)
}

s <- readline('Press enter...')
print(ZEST(makeStim=makeStimHelper(10,10)))

#r <- sapply(1:10, function(i) opiPresent(stim=makeStimHelper(10,10)(27,0)))
  

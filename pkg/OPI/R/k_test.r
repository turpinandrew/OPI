
#######################################################
# Helper function
# Stimulus is Size III 
#######################################################
k_test <- function() {

makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=0.43, color="white",
             duration=200, responseWindow=1500)
    class(s) <- "opiStaticStimulus"

    return(s)
}


require(OPI)
choose("KowaAP7000")

opiInitialise(ip="192.168.1.7", port=44965)

print(opiPresent(stim=makeStim(10,0), nextStim=NULL))

opiClose()

}


###ip <- "192.168.1.7"
###port <- 44965
###
###
###cat("Looking for server... ")
###suppressWarnings(tryCatch(    
###    v <- socketConnection(host = ip, port,
###                  blocking = TRUE, open = "w+b",
###                  timeout = 10)
###    , error=function(e) { 
###        stop(paste(" cannot find a server on port",port))
###    }
###))
###close(v)
###
###
###print("found server :)")
###
###socket <- tryCatch(
###    socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
###    error=function(e) stop(paste("Cannot connect to Kowa AP7000 on port", port))
###)
###
###msg <- paste0("OPI_SET_MODE 0")
###writeLines(msg, socket)
###res <- readLines(socket, n=1)
###    
###print(res)

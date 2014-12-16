
#ip <- "169.254.193.207"

ip <- "192.168.1.7"
port <- 44965


cat("Looking for server... ")
suppressWarnings(tryCatch(    
    v <- socketConnection(host = ip, port,
                  blocking = TRUE, open = "w+b",
                  timeout = 10)
    , error=function(e) { 
        stop(paste(" cannot find a server on port",port))
    }
))
close(v)


print("found server :)")

socket <- tryCatch(
    socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
    error=function(e) stop(paste("Cannot connect to Kowa AP7000 on port", port))
)

msg <- paste0("OPI_SET_MODE 0")
writeLines(msg, socket)
res <- readLines(socket, n=1)
    
print(res)

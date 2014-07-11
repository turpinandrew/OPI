sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("C:/Users/DOVS/Desktop/David/OPI/pkg/OPI/R")

chooseOpi("Octopus900")

opiInitialize(
  eyeSuiteJarLocation="C:/Program Files (x86)/Haag-Streit/EyeSuite/", 
  eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
  eye="left")

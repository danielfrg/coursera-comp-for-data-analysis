getmonitor <- function(id, directory, summarize = FALSE) {
  if (class(id) == "character"){
    data <- read.csv(file=paste("./", directory, "/", id, ".csv", sep=""),sep=",")
  }
  else{
    data <- read.csv(file=paste("./", directory, "/", sprintf("%03.f", id), ".csv", sep=""),sep=",")
  }
  if (summarize == TRUE) {
    print(summary(data))
  }
  return(data)
}
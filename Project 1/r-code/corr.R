corr <- function(directory, threshold = 0) {
  complete = complete(directory)
  pass_threshold = complete[complete$nobs > threshold, "id"]
  
  corr2 = function(id) {
    data = getmonitor(id, directory)
    c = cor(data["sulfate"], data["nitrate"], use = "complete.obs")
    return(c)
  }
  
  sapply(pass_threshold, corr2)
}
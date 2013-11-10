complete <- function(directory, id = 1:332) {
  l <- list()
  for (i in id) {
    l <- c(l, list(getmonitor(i, directory)))
  }
  vals <- sapply(l, function(x) min(colSums(!is.na(x[c("sulfate", "nitrate")]))) ) 
  data.frame(id = id, nobs = vals)
}
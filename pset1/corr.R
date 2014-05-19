corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  completes <- as.data.frame(complete(directory, 1:332))
  
  use_ids <- completes[completes$nobs > threshold,]$id
  
  # now we need to actually get the data for those ids
  files <- lapply(use_ids, function(i) {
    fname = sprintf("%s/%03d.csv", directory, i)
    f <- read.csv(fname)
  })
  
  my_d <- Reduce(function(x, y) { rbind(x, y)}, files)

  # only inlude completes!
  my_d <- subset(my_d, !is.na(sulfate) & !is.na(nitrate))

  result <- NULL
  for (i in use_ids) {
    obs <- my_d[my_d$ID == i,]
    result <- rbind(result, cor(obs$sulfate, obs$nitrate))
  }
  as.vector(result)
}
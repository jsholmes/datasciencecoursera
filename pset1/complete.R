complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- lapply(id, function(i) {
    fname = sprintf("%s/%03d.csv", directory, i)
    f <- read.csv(fname)
  })
  my_d <- Reduce(function(x, y) { rbind(x, y)}, files)
  
  # completes
  my_d <- subset(my_d, !is.na(sulfate) & !is.na(nitrate))
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  df <- NULL
  n = 0
  for (i in id) {
    tmp <- my_d[my_d$ID == i,]
    df <- rbind(df,c(i, length(tmp[,1])))
    
    n <- n + 1
  }
  colnames(df) <- c("id", "nobs")
  rownames(df) <- 1:n
  df
}
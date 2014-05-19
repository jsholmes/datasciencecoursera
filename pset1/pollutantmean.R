pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  # read in the files
  files <- lapply(id, function(i) {
    fname = sprintf("%s/%03d.csv", directory, i)
    f <- read.csv(fname)
  })
  
  my_d <- Reduce(function(x, y) { rbind(x, y)}, files)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  #my_d <- subset(d, is.element(ID, id))
  
  # get the column number they want
  col = which(colnames(my_d) == pollutant)
  
  my_d <- my_d[!is.na(my_d[col]),]
  
  v <- as.vector(my_d[col])
  return(mean(v[,1]))
}
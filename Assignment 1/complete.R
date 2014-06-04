complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  all <- data.frame(id=numeric(), nobs=numeric())
  for (i in id) {
    data <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=""))
    good <- complete.cases(data)
    data <- data[good, ]  # remove rows with any NA values
    all <- rbind(all, data.frame(id = i, nobs = nrow(data)))
  }
  all
}
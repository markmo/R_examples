corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  nobs <- complete(directory)
  ret = c()
  for (file in list.files(directory)) {
    data <- read.csv(paste(directory, "/", file, sep=""))
    good <- complete.cases(data)
    data <- data[good, ]
    id <- as.numeric(strsplit(file, ".", fixed=TRUE)[[1]][1])
    if (nobs[nobs$id == id, ]$nobs > threshold) {
      ret <- append(ret, cor(data$sulfate, data$nitrate))
    }
  }
  ret
}
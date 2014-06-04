rankall <- function(outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(outcomes$State)
  if (state %in% states) {
    outcome.cols <- c(11, 17, 23)
    names(outcome.cols) <- c("heart attack", "heart failure", "pneumonia")
    if (outcome %in% names(outcome.cols)) {
      col <- outcome.cols[[outcome]]
    } else {
      stop("invalid outcome")
    }
    outcomes[, col][outcomes[, col] == "Not Available"] <- NA
    sub <- outcomes[!is.na(outcomes[, col]), ]
    sub[, col] <- as.numeric(sub[, col])
    splits <- split(sub, sub$State)
    bystate <- sapply(splits, function(x) {
      sorted <- x[order(x[, col], x$Hospital.Name), ]$Hospital.Name
      if (num == "best") {
        head(sorted, 1)
      } else if (num == "worst") {
        tail(sorted, 1)
      } else {
        sorted[num]
      }
    })
    df <- data.frame(bystate, names(bystate))
    names(df) <- c("hospital", "state")
    df
  } else {
    stop("invalid state")
  }
}

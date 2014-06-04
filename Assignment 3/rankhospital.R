rankhospital <- function(state, outcome, num = "best") {
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
    stateOutcomes <- splits[[state]]
    sorted <- stateOutcomes[order(stateOutcomes[, col], stateOutcomes$Hospital.Name), ]
    if (num == "best") {
      head(sorted, 1)$Hospital.Name
    } else if (num == "worst") {
      tail(sorted, 1)$Hospital.Name
    } else {
      sorted[num, ]$Hospital.Name
    }
  } else {
    stop("invalid state")
  }
}

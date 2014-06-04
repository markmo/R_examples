best <- function(state, outcome) {
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
    sort(stateOutcomes[which.min(stateOutcomes[, col]), ]$Hospital.Name)[1]
  } else {
    stop("invalid state")
  }
}
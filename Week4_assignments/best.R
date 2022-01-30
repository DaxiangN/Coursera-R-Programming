best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        df <- df[c(2,7,11,17,23)]
        outcomes <- c("heart attack","heart failure","pneumonia")
        names(df) <- c("name","state","heart attack","heart failure","pneumonia")
        ## Check that state and outcome are valid
        if (any(df$state == state) == FALSE) {
                stop("invalid state")
        } else if (any(outcomes == outcome) == FALSE) {
                stop("invalid outcome")
        } else {
                ## Return hospital name in that state with lowest 30-day death rate
                df <- df[df$state == state&df[[outcome]] != "Not Available",]
                df[[outcome]] <- as.numeric(df[[outcome]])
                hospital.name <- sort(df$name[which(df[[outcome]] == min(df[[outcome]]))])[1]
                return(hospital.name)
        }
}
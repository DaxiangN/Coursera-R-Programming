rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        df <- df[c(2,7,11,17,23)]
        outcomes <- c("heart attack","heart failure","pneumonia")
        names(df) <- c("name","state","heart attack","heart failure","pneumonia")
        ## Check that state and outcome are valid
        if (any(outcomes == outcome) == FALSE) {
                stop("invalid outcome")
        } else {
                ## For each state, find the hospital of the given rank
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                rankhospital <- function(df, outcome, num = "best") {
                        df <- df[df[[outcome]] != "Not Available",]
                        df[[outcome]] <- as.numeric(df[[outcome]])
                        df<- df[order(df$name),]
                        list <- df$name[order(df[[outcome]])]
                        if (num == "best") return(list[1])
                        if (num == "worst") return(list[length(list)])
                        else return(list[num])
                }
                df <- split(df, df$state)
                hospital.names <- sapply(df, rankhospital, outcome = outcome, num = num)
                list <- cbind(hospital = hospital.names, state = names(hospital.names))
                list <- as.data.frame(list)
                return(list)
        }
}
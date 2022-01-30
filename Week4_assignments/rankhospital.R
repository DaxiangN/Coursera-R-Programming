rankhospital <- function(state, outcome, num = "best") {
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
        } else { ## Return hospital name in that state with the given rank
                 ## 30-day death rate
                df <- df[df$state == state&df[[outcome]] != "Not Available",]
                df[[outcome]] <- as.numeric(df[[outcome]])
                df<- df[order(df$name),]
                list <- df$name[order(df[[outcome]])]
                if (num == "best") return(list[1])
                if (num == "worst") return(list[length(list)])
                else return(list[num])
        }
}
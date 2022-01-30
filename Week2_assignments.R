pollutantmean <- function(directory, pollutant, id = 1:332) {
        file_list <- list.files(directory, full.names = TRUE) # pull out the names of the csv files
        df <- data.frame()
        for (i in id) {
                df <- rbind(df, read.csv(file_list[i])) # import csv files one by one and combine them together
        }
        mean(df[!is.na(df[pollutant]),][,pollutant]) # calcualte the mean
}

complete <- function(directory, id = 1:332) {
        file_list <- list.files(directory, full.names = TRUE)
        df <- data.frame() # create an empty data frame
        for (i in id){
                y <- read.csv(file_list[i])
                x <- nrow(y[complete.cases(y),]) # calculate the number of rows for complete observations
                df1 <- data.frame("id" = i, "nobs" = x) # create the data frame with id and nobs
                df <- rbind(df,df1) # add in the id and nobs value to the data frame
        }
        return(df)
}

corr <- function(directory, threshold = 0) {
        file_list <- list.files(directory, full.names = TRUE)
        cor1 <- c() # create an empty vector
        for (i in 1:332) {
                y <- read.csv(file_list[i])
                x <- nrow(y[complete.cases(y),])
                if (x > threshold) {
                        df1 <- y[complete.cases(y),]
                        cor2 <- cor(df1[,"sulfate"],df1[,"nitrate"]) # calculate the correlation between sulfate and nitrate
                        cor1 <- c(cor1, cor2) # combine the calcuated value with previous results
                }
                
        }
        return(cor1)
}

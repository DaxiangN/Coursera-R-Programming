# read csv file
x <- read.csv("hw1_data.csv")
#In the dataset provided for this Quiz, what are the column names of the dataset? 
colnames(x)
# Extract the first 2 rows of the data frame and print them to the console. What does the output look like?
head(x)
# How many observations (i.e. rows) are in this data frame?
dim(x) # or nrow(x) # to learn the number of column use ncol()
# Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
tail(x)
# What is the value of Ozone in the 47th row?
x[47,1] # x[row number,col number]
# How many missing values are in the Ozone column of this data frame?
sum(is.na(x[,1])) # False = 0, true = 1, so when sum together you will get the number of TRUE, which is the number of missing value
# What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(x[!is.na(x[,1]),][,1])
# Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
nx <- x[complete.cases(x),] # remove all rows with missing value
mean(nx[nx[,1] > 31 & nx[,4] > 90,][,2]) # extract the rows with Ozone is above 31 and Temp is above 90. & is logical AND, | is logical OR, && is similar with & but only gives you one value, same with ||.
# What is the mean of "Temp" when "Month" is equal to 6? 
mean(x[x[,5] == 6,][,4])
# What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
bad <- is.na(x[x[,5] == 5,][,1]) # Find the NA elements
max(x[x[,5] == 5,][,1][!bad]) # remove the NA elements in interested rows
#Programming 1 - Assignment 1
#Your name(s): Group 9 - Mike Johnson | Ryan Mathis

#The data of over 1000 eBay auctions is provided in the file eBayAcution.csv. 
#Use RStudio to study this marketplace. 
#(Source: The data is adapted from this book: https://www.dataminingbook.com/book/r-2nd-edition-2023)

#1) Load the file: "eBayAcution.csv" and save it as auctionData.
auctionData = read.csv("eBayAuctions.csv")


#2) Write a code that checks if the dataset has any missing values, 
#   a code that returns the number of auctions (i.e., rows), 
#   and one to return the number of variables (i.e., columns).
na_check = anyNA(auctionData)
num_rows = nrow(auctionData)
num_cols = ncol(auctionData)

cat(
  "Are there any missing values? ", na_check, "\n",
  "Number of Auctions: ", num_rows, "\n",
  "Number of Columns: ", num_cols, "\n"
)


#3) What is the maximum auction duration? How many auctions were open for these many days? 
#   What is the average auction duration? What percentage of the auctions have an above average duration?
max_duration = max(auctionData$Duration)
count_max_duration = sum(auctionData$Duration == max_duration)
avg_duration = mean(auctionData$Duration)
count_above_avg_duration = sum(auctionData$Duration > avg_duration)
percent_above_avg_duration = count_above_avg_duration / num_rows

cat(
  "What is the maximum auction duration? ", max_duration, "\n",
  "How many auctions were open for these many days? ", count_max_duration, "\n",
  "What is the average auction duration? ", avg_duration, "\n",
  "What percentage of the auctions have an above average duration?", percent_above_avg_duration
)

#4) Create a new variable called Ratio that calculates the ratio of the closing price over the opening price 
#   for each auction and add this variable to the dataset as a new column. 
#   What's the average ratio of all auctions? What's the average ratio of 'Computer' auctions?
Ratio = auctionData$ClosePrice / auctionData$OpenPrice
auctionData$Ratio = Ratio
avg_ratio = mean(auctionData$Ratio)
avg_ratio_computer = mean(auctionData$Ratio[auctionData$Category == "Computer"])

cat(
  "What's the average ratio of all auctions? ", avg_ratio, "\n",
  "What's the average ratio of 'Computer' auctions? ", avg_ratio_computer, "\n"
)

#5) Create an object named "catNames" that contains the names of unique auction categories, 
#   sorted in alphabetical order. Write a code to return the number of categories stored in this object.
catNames = sort(unique(auctionData$Category))
count_categories = length(catNames)
count_categories

cat("Number of categories: ", count_categories)

#6) Write a loop to go through "catNames" and calculate the number of auctions in each category. 
#   In so doing, save the results in a vector called "numAuctions". 
#   Write a code to return the values stored in this object.

numAuctions = rep(NA, length(catNames))

for( i in 1:length(catNames)) {
  numAuctions[i] = sum(auctionData$Category == catNames[i])
}

numAuctions

#7) Combine the two objects (catNames and numAuctions) into a new data frame called catInfo. 
# Write two different codes to return the fifth element of the second column in the catInfo dataframe.

catInfo = data.frame(catNames, numAuctions)

catInfo[5, 2]
catInfo[catInfo$catNames == "Computer", 2]

#8) Write a piece of code that prints the name of each category and the number of auctions in that category.

catInfo


#9) Create a function, called weekendTest, that checks whether a given day is a weekend (endDay of 'Sat' or 'Sun') 
#   or not and returns TRUE or FALSE (logical constants in R).
#   Then use this function to create a new variable (called Weekend) that shows if each auction had an endDay of the weekend or not. 
#   Add this variable to the dataset as a new column. How many auction ended on weekend? (Write a code that returns this value)

# Function to test whether a given day is a weekend
weekendTest = function(end_day) {
  if(end_day == "Sat" | end_day == "Sun") {TRUE} else {FALSE}
}

# Creat 'Weekend' column and apply weekendTest function to the data frame
auctionData$Weekend = rep(NA, length(auctionData$endDay))

for(i in 1:length(auctionData$endDay)) {
  auctionData$Weekend[i] = weekendTest(auctionData$endDay[i])
}

# Summarize weekend auctions
weekend_auctions = sum(auctionData$Weekend)

cat("How Many auctions ended on the weekend? ", weekend_auctions)

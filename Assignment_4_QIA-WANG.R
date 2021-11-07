#1) Load the price index .csv file attached via this assignment#########################
destfile<- "D:\\One\\OneDrive\\My research\\5th semester\\R\\Assignment 4\\food-price-index-September-2021-index-numbers-csv-tables.csv"
pricedata<- read.csv(destfile) #load the file

#2) Use 4 methods that you learned in the last two sessions to manipulate the dataset####
#2.1: read the data file and overview its content (library(data.table))
library(data.table)
head(pricedata,n=3) # check the first 3 rows
tail(pricedata,n=10)# check the last 10 rows
summary(pricedata) # summary of the object
dim(pricedata) # check the dimension
names(pricedata) # check the object names
str(pricedata)# the structure
#attributes(pricedata)# object's attributes
hist(pricedata$Data_value)# Use a histogram to display data distribution
table(pricedata$Data_value)[1:5] # Frequency of occurrence of the first 5 values
is.factor(pricedata$Series_title_1) # Determine whether it is factor data
#as.factor(pricedata$Series_title_1) # Convert to factor data

#2.2: Remove the missing data
colSums(is.na(pricedata))
#2.2.1 Method 1: na.omit()
good1<-na.omit(pricedata)
dim(good1)
#2.2.2 Method 2: complete.cases()
good2<-pricedata[complete.cases(pricedata),] 
dim(good2)
#2.2.3 Method 3: is.na()
badrow<-which(rowSums(is.na(pricedata))>0) # Find the rows with missing values in the table "pricedata"
bad<-pricedata[badrow,] # Save these rows with missing values in a table "bad"
good3<-pricedata[-badrow,] # Save rows without missing values in the original table
dim(good3)

#2.3: Modify table
#2.3.1 Change the factor name
names(good3)[1]<-"reference" # Change the factor name through the names() function
names(good3)[1]<-"Series_reference" #Change it back

#2.3.2 Sorting
sordata<-sort(good1$Data_value,decreasing=TRUE)
head(sordata)

#2.3.3 Ordering
#Method 1: order
ordata<-good1[order(good1$Series_reference,good1$Data_value),]
head(ordata)
#Method 2: library(plyr)
head(arrange(good1,Series_reference))

#2.3.4 Adding new column
#Method 1
newdata<-transform(good1,price=(Data_value*100))# Add new column named "price"
head(newdata,n=3)
#Method 2: 
ID<-1:25881
df<-data.frame(ID,good3)# Add serial number column
head(df,n=3)
#2.4: Subsetting the data set
#2.4.1 Remove the unwanted columns
newdata1<- good3[,-c(4:7)]# Remove columns with unique values
head(newdata1,n=3)
#2.4.2 Select the desired column with conditions
# Method 1: Designated columns
newdata2<-good3[,c(1:3,8)]#Specify columns 1 to 3 and column 8
head(newdata2,n=3)
# Method 2: Column containing key information
Olives<-pricedata[pricedata$Series_title_1=="Olives, jar, 400g",] # All rows where Series_reference is "Olives, jar, 400g"
head(Olives,n=2)
# Method 3: The column containing the specified value
ndata<-newdata[newdata$price<=50 | newdata$price>=500,]#Columns less than or equal to 50, or greater than or equal to 500
head(ndata,n=2)
# Method 4: The column containing the specified charactors
ndata<-good3[good3$Period %in% c("2021.09"),] # %in%
head(ndata,n=2)
# Method 5: Casting data frames:library(reshape2)
newdata3<-dcast(good3,Data_value~Series_reference)
newdata3[1:3,1:5]
library(xlsx)# save the results into xlsx file
write.xlsx(newdata3,"D:\\One\\OneDrive\\桌面\\newdata3.xlsx",sheetName="newdata3",append=TRUE)

#3)Use the factor function for column "Series_title_1" and get the average for each product using the price values in column "Data_value" by sapply function####
#Method 1:
price1<-data.frame() # create a empty dataframe
n=1
fac<-factor(newdata2$Series_title_1,ordered=TRUE) # extract the factor names
while (n<=length(levels(fac))){
  fac1<-newdata2[(newdata2$Series_title_1 %in% c(levels(fac)[n])),] # search the factor names in Series_title_1 one by one
  x <- list(fac1$Data_value) # save the search results into x
  price1<-rbind(c(levels(fac)[n],sapply(x, FUN = mean)),price1) #using sapply to calculate the average price, then using rbind to save the product name and its price into dataframe price1
  n=n+1
}
names(price1)<-c("Product","Average Price") # set the columns names
price1$No.<-1:length(price1$Product)# generate a index column
library(dplyr)
price1 <-select(price1, "No.", "Product", "Average Price")# rearrange the order of columns
price1 # display the final results
#Method 2:
splitmean <- function(newdata2) { #build a function by split and sapply function
  s <- split( newdata2, newdata2$Series_title_1) # split the data by Series_title_1
  sapply( s, function(x) mean(x$Data_value) )# calculate the average price
}
price<-splitmean(newdata2) # call the function
# display the final results
price
#4) Push the r file into your GitHub like before and submit your GitHub link like prior assignments####
#When you read this, I have finished uploading.     

#Thanks for your patience!
  
#THE END



library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)

# Get the destination of the CSV file
setwd("C:/Users/Owner/Desktop/CMSC462/hw4")

# Read in the csv file
BreadBasket <- read.csv("BreadBasket_DMS.csv")

# Removes rows with missing values
BreadBasket <- BreadBasket[complete.cases(BreadBasket), ]

# Removes rows where the item is "NONE"
BreadBasket <- BreadBasket[BreadBasket$Item != "NONE",]

# Using the table function to count item occurrences
item_counts <- table(BreadBasket$Item)

item_counts


# Gives each member variable of Item its own level
BreadBasket$Item = as.factor(BreadBasket$Item)

glimpse(BreadBasket)

# Use Transaction Number to separate the orders

#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(BreadBasket,c("Transaction"),
                         function(df1)paste(df1$Item, collapse = ","))

#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData

#Set Transaction number to NULL
transactionData$Transaction <- NULL
#Rename column to "items"
colnames(transactionData) <- c("items")


head(transactionData, 10)

write.csv(transactionData,"bread_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written

#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions("bread_basket_transactions.csv", format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

tr
summary(tr)
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
#mar - A numeric vector of length 4, which sets the margin sizes in the 
#following order: bottom, left, top, and right. 
#The default is c(5.1, 4.1, 4.1, 2.1).
par(mar=c(0,10,1,.1))
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
association.rules <- apriori(tr, parameter = list(supp=0.0025, conf=0.2,maxlen=3))

# Filter out any rule with "Coffee" on the right-hand side
association.rules <- subset(association.rules, !(rhs %in% "Coffee"))



summary(association.rules)

inspect(association.rules)
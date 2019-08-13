library(stringr)
library(igraph)

testData <- read.csv(file="/Users/catherineding/Desktop/testData.csv",head=TRUE,sep=",")
testData <- testData[!apply(is.na(testData) | testData == "", 1, all),]
testDataCopy <- testData

edgeFrom.list <- vector("list", 1)
edgeFrom.list[[1]] <- NULL

edgeTo.list <- vector("list", 1)
edgeTo.list[[1]] <- NULL

node.list <- vector("list", 1)
node.list[[1]] <- NULL


color.list <- vector("list", 6)
color.list[[1]] <- "green"
color.list[[2]] <- "blue"
color.list[[3]] <- "red"
color.list[[4]] <- "yellow"
color.list[[5]] <- "purple"
color.list[[6]] <- "black"
colors.list <- vector ("list", 1)
colors.list[[1]] <- NULL

lines.list <- vector ("list", 1)
lines.list[[1]] <- NULL

doubleCount.list <- vector ("list", 1)
doubleCount.list[[1]] <- NULL

firstCount <- 0
duplicateCount <- 0
line <- 0


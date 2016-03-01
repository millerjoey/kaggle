setwd("/home/joey/Desktop/Kaggle") # Desktop
setwd("Desktop/Kaggle/") # Mac
train <- read.csv(file = "train.csv", as.is=T)


#============Plots============
library(ggplot2)
# Looking at the numbers of examples above each (relevancy) score
y <- vector(mode = "numeric", length=5)
x <- seq(from = 1, to = 3, length.out=5)
index <- 1
for (i in x) {
  y[index] <- sum(train$relevance >= i)
  index <- index + 1
}
qplot(x, y, ylim=c(0, 75000), ylab="Number of Examples Above Score", xlab="Relevance Score")

# Histogram of the same
relevance <- train$relevance
qplot(relevance, geom="histogram", bins=15)
#============Plots=============


#========Create Dictionary=====
# Get important words, defined through some cutoff value for a lowest relevance.
# Partition dataset by cutoff:
train3 <- train[train$relevance>=3,]
train2 <- train[train$relevance<3 & train$relevance>=2,]
train1 <- train[train$relevance<2 & train$relevance>=1,]

View(train3)

getTitle <- function(data) {
  splitby <- c(" ")
  product <- tolower(paste(unlist(strsplit(data[3], split = "-")), collapse = " "))
  product <- unlist(strsplit(product, split=" "))
  return(product)
}

titles3 <- apply(train3, MARGIN = 1, getTitle)

getSearch <- function(data) {
  splitby <- c(" ")
  product <- tolower(paste(unlist(strsplit(data[4], split = "-")), collapse = " "))
  product <- unlist(strsplit(product, split=" "))
  return(product)
}

searches3 <-  apply(train3, MARGIN = 1, getSearch)

max(unlist(lapply(X=searches3, length)))
max(unlist(lapply(X=titles3, length)))

# This just copies every intersection into the empty matrix. Second loop because some intersections have multiple elements.
intersectionMat <- function(searches, titles) {
  words <- matrix(nrow = length(searches), ncol = max(c(max(unlist(lapply(X=titles, length))), max(unlist(lapply(X=searches, length)))))) # Empty matrix to put intersections into.
  for (i in 1:length(searches)) {
    intersect <- intersect(searches[[i]], titles[[i]])
    if (length(intersect)==0) {
      words[i,1] <- NA
    }
    else {
      for (j in 1:length(intersect)) {
        words[i,j] <- intersect[j]
      }
    }
  }
  return(words)
}

words3 <- intersectionMat(searches3, titles3)

sum(!is.na(words3[,1]))/length(searches3) # 17279/19125=90.34% of 3s have common words.

words3Vec <- as.vector(x = words3, mode = "character") # Unwind matrix into vector
words3Vec <- words3Vec[is.na(words3Vec)==F]             # Get rid of empties
length(unique(words3Vec))                             # 2896 unique words. This is our dictionary.

# wordsVec now contains all copies of words that are common to the search string and the product.

table <- table(wordsVec) # Don't go down this rabbit hole. Some items are more popular than others, so they are represented more frequently.

#==========================================================
# I want to see the frequency that these words occur in for
# the other relevance scores. Hopefully the freq correlates 
# with relevance.
#==========================================================

# Train2
titles2 <- apply(train2, MARGIN = 1, getTitle)
searches2 <-  apply(train2, MARGIN = 1, getSearch)

words2 <- intersectionMat(searches = searches2, titles = titles2)
sum(!is.na(words2[,1]))/length(searches2) # 86.12% have common words.

#train1
titles1 <- apply(train1, MARGIN = 1, getTitle)
searches1 <-  apply(train1, MARGIN = 1, getSearch)

words1 <- intersectionMat(searches=searches1, titles = titles1)
sum(!is.na(words1[,1]))/length(searches1) # 73.88 % have common words.



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
qplot(x, y, ylim=c(0, 75000), ylab="Number of Examples geq Each Score", xlab="Relevance Score")

# Histogram of the same
relevance <- train$relevance
qplot(relevance, geom="histogram", bins=15)
#============Plots=============


#========Create Dictionary=====
# Get important words, defined through some cutoff value for a lowest relevance.
# Partition dataset by cutoff:
index3 <- train$relevance>=3
index2 <- train$relevance<3 & train$relevance>=2
index1 <- train$relevance<2 & train$relevance>=1

splitDataBy <- function(data, charVector, index) {
  x <- data[index]
  for (split in charVector) {
    x <- tolower(unlist(strsplit(as.character(x), split)))
    x <- x[!x==""]
  }
  return(x) # Remove empty values
}

titles <- apply(train, MARGIN = 1, splitDataBy, charVector=c(" ", "-", ","), index=3)
searches <-  apply(train, MARGIN = 1, splitDataBy, charVector=c(" ", "-", ","), index=4)


max(unlist(lapply(X=searches[index3], length)))
max(unlist(lapply(X=titles[index3], length)))

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

words <- intersectionMat(searches, titles)

sum(!is.na(words[,1]))/length(searches) # 85% of have common words.

# Words with relevance=3
words3Vec <- as.vector(x = words[index3,], mode = "character") # Unwind matrix into vector
words3Vec <- words3Vec[is.na(words3Vec)==F]                    # Get rid of empties
length(unique(words3Vec))                                      # 2896 unique words. This was our dictionary, according to the original plan. I ended up just using # common words as the predictor.

# wordsVec now contains all copies of words that are common to the search string and the product.

#==========================================================
# Parameter Estimates
#==========================================================

# Number of Matching words:
fiveMatches <- apply(words, MARGIN=1, function(x) sum(!is.na(x))>4)
avg5 <- mean(train[fiveMatches,5])

fourMatches <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==4)
avg4 <- mean(train[fourMatches,5])

threeMatches <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==3)
avg3 <- mean(train[threeMatches,5])

twoMatches <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==2)
avg2 <- mean(train[twoMatches,5])

oneMatch <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==1)
avg1 <- mean(train[oneMatch,5])

noMatch <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==0)
avg0 <- mean(train[noMatch,5])

avgs <- c(avg0, avg1, avg2, avg3, avg4, avg5)

# Length of Search Phrase:
searchLengths <- sapply(searches, FUN = length)

sLength1 <- searchLengths == 1
mean(train[sLength1,5])

sLength2 <- searchLengths == 2
mean(train[sLength2,5])

sLength3 <- searchLengths == 3
mean(train[sLength3,5])

sLength45 <- searchLengths == 4 | searchLengths == 5
mean(train[sLength45,5])

sLength678 <- searchLengths == 6 | searchLengths == 7 | searchLengths == 8
mean(train[sLength678,5])
var(train[sLength678,5])

sLength9 <- searchLengths == 9
mean(train[sLength9,5])
var(train[sLength9,5])

#====================================
# Predictions
#====================================

computeRMS <- function(predictions, data) {
  return(sqrt(sum((predictions-data$relevance)^2)/length(predictions)))
}

predict <- function(wordsRow) {
  if (length(wordsRow[!is.na(wordsRow)]) > 5) {prediction <- avgs[6]}
  else {prediction <- avgs[length(wordsRow[!is.na(wordsRow)])+1]}
  return(prediction)
}

predictions <- apply(words, MARGIN=1, FUN = predict)

computeRMS(predictions,train) # RMS is 0.525997

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
# Preliminary Predictions
#==========================================================
predict <- function(wordsRow) {
  if (length(wordsRow[is.na(wordsRow)==F])>1) {
    prediction <- 2.448595
  }
  if (length(wordsRow[is.na(wordsRow)==F])==1) {
    prediction <- 2.352597
  }
  if (length(wordsRow[is.na(wordsRow)==F])==0) {
    prediction <- 2.185824
  }
  return(prediction)
}

# to beat: 2.448595, 2.352597, 2.185824 => 0.525997

# To optimize the assignments, I used the average proportions according to each partition by relevance.
twoMatches <- apply(words, MARGIN=1, function(x) sum(!is.na(x))>1)
mean(train[twoMatches,5])

oneMatch <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==1)
mean(train[oneMatch,5])

noMatch <- apply(words, MARGIN=1, function(x) sum(!is.na(x))==0)
mean(train[noMatch,5])

computeRMS <- function(predictions, data) {
  return(sqrt(sum((predictions-data$relevance)^2)/length(predictions)))
}

predictions <- apply(words, MARGIN=1, FUN = predict)

computeRMS(predictions,train) # RMS is 0.525997

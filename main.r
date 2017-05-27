library(tm)
library(stringr)
library(wordnet)
library(randomForest)
setDict("/usr/share/wordnet/dict")
Sys.setenv(WNHOME = "/usr/share/wordnet")
stopifnot(initDict())

transformCorpus <- function(reviews){
  reviews <- tm_map(reviews, stripWhitespace)
  reviews <- tm_map(reviews, content_transformer(tolower))
  f <- content_transformer(function(x, pattern, sub) gsub(pattern, sub, x))
  reviews <- tm_map(reviews, f, "'[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'", " ")
  reviews <- tm_map(reviews, f, "  ", " ")
  reviews <- tm_map(reviews, removeWords, stopwords("english"))
  ##reviews <- tm_map(reviews, stemDocument)
  return(reviews)
}

calculateDistance <- function(X){
  for(id in 1:nrow(X)){
    questionCorpus <- VCorpus(VectorSource(c(X$question1[id], X$question2[id])))
    questionCorpus <- transformCorpus(questionCorpus)
    dtm <- DocumentTermMatrix(questionCorpus)
    if(sum(dtm[1,]$i) == 0){
      print(X$question1[id])
      print(dtm[1,])
    }
    if(sum(dtm[2,]$i) == 0){
      print(X$question2[id])
      print(dtm[2,])
    }
    dtm2 <- weightTfIdf(dtm)
    X$distance[id] <- dist(dtm2)
  }
  X
}

train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
train <- na.exclude(train)
test <- na.exclude(test)
set.seed(2017)
trainSample <- train[sample(nrow(train), 20000),]
testSample <- train[sample(nrow(test), 100000),]

for(id in 1:nrow(trainSample)){
  questionCorpus <- VCorpus(VectorSource(c(trainSample$question1[id], trainSample$question2[id])))
  questionCorpus <- transformCorpus(questionCorpus)
  dtm <- DocumentTermMatrix(questionCorpus)
  if(sum(dtm[1,]$i) == 0){
    print(trainSample$question1[id])
    print(dtm[1,])
  }
  if(sum(dtm[2,]$i) == 0){
    print(trainSample$question2[id])
    print(dtm[2,])
  }
  dtm2 <- weightTfIdf(dtm)
  trainSample$distance[id] <- dist(dtm2)
}

for(id in 1:nrow(testSample)){
  questionCorpus <- VCorpus(VectorSource(c(testSample$question1[id], testSample$question2[id])))
  questionCorpus <- transformCorpus(questionCorpus)
  dtm <- DocumentTermMatrix(questionCorpus)
  if(sum(dtm[1,]$i) == 0){
    print(testSample$question1[id])
    print(dtm[1,])
  }
  if(sum(dtm[2,]$i) == 0){
    print(testSample$question2[id])
    print(dtm[2,])
  }
  dtm2 <- weightTfIdf(dtm)
  testSample$distance[id] <- dist(dtm2)
}
trainSampleSimple <- trainSample[,c("distance", "is_duplicate")]
trainSampleSimple$is_duplicate <- as.factor(trainSampleSimple$is_duplicate)
testSampleSimple <- testSample[,c("distance", "is_duplicate")]
testSampleSimple$is_duplicate <- as.factor(testSampleSimple$is_duplicate)

m <- randomForest(is_duplicate ~ ., trainSampleSimple)
res <- predict(m, testSampleSimple)
tables <- table(res, testSampleSimple$is_duplicate)
error <- 1-(sum(diag(tables)))/sum(tables)




dtm <- DocumentTermMatrix(questionCorpus)

inspect(dtm)
dtm2 <- weightTfIdf(dtm)

filter <- getTermFilter("StartsWithFilter", "car", TRUE)
sapply(getIndexTerms("VERB", 5, filter), getLemma)
filter <- getTermFilter("ExactMatchFilter", dtm$dimnames$Terms[4], TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "@")
sapply(related, getWord)

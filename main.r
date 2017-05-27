library(tm)
library(stringr)
library(wordnet)
library(randomForest)
library(text2vec)
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
testSample <- train[sample(nrow(train), 100000),]

for(id in 1:nrow(trainSample)){
  questionCorpus <- VCorpus(VectorSource(c(trainSample$question1[id], trainSample$question2[id])))
  questionCorpus <- transformCorpus(questionCorpus)
  dtm <- DocumentTermMatrix(questionCorpus)
  if(sum(dtm[1,]$i) == 0){
    print(trainSample$question1[id])
    print(dtm[1,])
    trainSample <- trainSample[-id,]
  }
  else{
    if(sum(dtm[2,]$i) == 0){
      print(trainSample$question2[id])
      print(dtm[2,])
      trainSample <- trainSample[-id,]
    }
    else{
      dtm2 <- weightTfIdf(dtm)
      trainSample$distance[id] <- dist(dtm2)
      similarity <- sim2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      distance <- dist2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      trainSample$sim2[id] <- sum(similarity)/nrow(similarity)
      trainSample$dist2[id] <- sum(distance)/nrow(distance)
    }
  }
}

for(id in 1:nrow(testSample)){
  questionCorpus <- VCorpus(VectorSource(c(testSample$question1[id], testSample$question2[id])))
  questionCorpus <- transformCorpus(questionCorpus)
  dtm <- DocumentTermMatrix(questionCorpus)
  if(sum(dtm[1,]$i) == 0){
    print(testSample$question1[id])
    print(dtm[1,])
    testSample <- testSample[-id,]
  }
  else{
    if(sum(dtm[2,]$i) == 0){
      print(testSample$question2[id])
      print(dtm[2,])
      testSample <- testSample[-id,]
    }
    else{
      dtm2 <- weightTfIdf(dtm)
      testSample$distance[id] <- dist(dtm2)
      similarity <- sim2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      distance <- dist2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      testSample$sim2[id] <- sum(similarity)/nrow(similarity)
      testSample$dist2[id] <- sum(distance)/nrow(distance)
    }
  }
}
trainSampleSimple <- trainSample[,c("distance", "dist2", "sim2", "is_duplicate")]
trainSampleSimple$is_duplicate <- as.factor(trainSampleSimple$is_duplicate)
testSampleSimple <- testSample[,c("distance", "dist2", "sim2", "is_duplicate")]
testSampleSimple$is_duplicate <- as.factor(testSampleSimple$is_duplicate)

m <- randomForest(is_duplicate ~ ., trainSampleSimple)
res <- predict(m, testSampleSimple)
tables <- table(res, testSampleSimple$is_duplicate)
error <- 1-(sum(diag(tables)))/sum(tables)



######################################
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
#############################

for(id in 1:nrow(trainSample)){
  questionCorpus <- VCorpus(VectorSource(c(trainSample$question1[id], trainSample$question2[id])))
  questionCorpus <- transformCorpus(questionCorpus)
  dtm <- DocumentTermMatrix(questionCorpus)
  if(sum(dtm[1,]$i) == 0){
    print(trainSample$question1[id])
    print(dtm[1,])
    trainSample <- trainSample[-id,]
  }
  else{
    if(sum(dtm[2,]$i) == 0){
      print(trainSample$question2[id])
      print(dtm[2,])
      trainSample <- trainSample[-id,]
    }
    else{
      dtm2 <- weightTfIdf(dtm)
      trainSample$distance[id] <- dist(dtm2)
      similarity <- sim2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      distance <- dist2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
      trainSample$sim2[id] <- sum(similarity)/nrow(similarity)
      trainSample$dist2[id] <- sum(distance)/nrow(distance)
      count <- 0
      for(i in 1:length(dtm[1,])){
        if(dtm[1,][i] >0 && dtm[2,][i]>0){
          count <- count + 1
        }
      }
      #trainSample$length1[id] <- sum(dtm[1,])
      #trainSample$length2[id] <- sum(dtm[2,])
      #trainSample$shared[id] <- count
    }
  }
}

######################################
for(id in 1:nrow(t))
question1 <- train$question1[1]
question2 <- train$question2[1]
tokens <- list(question1, question2) %>% tolower() %>% word_tokenizer()
v = create_vocabulary(itoken(tokens), stopwords = stopwords("english"))
corpus = create_corpus(itoken(tokens), vocab_vectorizer(v, skip_grams_window = 3))
dtm = get_dtm(corpus)
tcm = get_tcm(corpus)
glove_model = GlobalVectors$new(word_vectors_size = 3, max_cost=5, vocabulary = v, x_max = 2)
fit(tcm, glove_model, n_iter = 20)
wv <- glove_model$get_word_vectors()
#rwmd_model = RelaxedWordMoversDistance$new(wv)
#rwmd_dist = dist2(as.matrix(dtm[1, ]), as.matrix(dtm[2, ]), method = rwmd_model, norm = 'none')
similarity <- sim2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
distance <- dist2(x = as.matrix(dtm[1,]), y= as.matrix(dtm[2,]), norm = 'none')
sum(similarity)/nrow(similarity)
sum(distance)/nrow(distance)
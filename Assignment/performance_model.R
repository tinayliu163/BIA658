# sentiment analysis of movie reviews
# test the performance of a baseline model 

install.packages(c('tm', 'SnowballC', 'wordcloud'))
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
rm(list=ls())
reviews = read.csv(file = "/Users/admin/Downloads/movie_reviews.csv", stringsAsFactors = F, row.names = 1)

# A collection of text documents is called a Corpus
review_corpus = Corpus(VectorSource(reviews$content))

# Remove numbers
review_corpus = tm_map(review_corpus, removeNumbers)

# Remove punctuation marks and stopwords
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords,c("duh", "whatever",  stopwords("english")))

# Remove extra whitespaces
review_corpus = tm_map(review_corpus, stripWhitespace)

# Precompiled list of words with positive and negative meanings
neg_words = read.table(file = "/Users/admin/Downloads/negative-words.txt", header = F, stringsAsFactors = F)[, 1]
pos_words = read.table("/Users/admin/Downloads/positive-words.txt", header = F, stringsAsFactors = F)[, 1]

# neg, pos contain the number of positive and negative words in each document
reviews$neg = sapply(review_corpus, tm_term_score, neg_words)
reviews$pos = sapply(review_corpus, tm_term_score, pos_words)

# remove the actual texual content for statistical models
reviews$content = NULL

# construct the dataset for models
reviews$polarity = as.factor(reviews$polarity)

# Split to testing and training set
id_train <- sample(nrow(reviews),nrow(reviews)*0.80)
reviews.train = reviews[id_train,]
reviews.test = reviews[-id_train,]

# Calculate misclassification rate
pred=ifelse (reviews.test$pos > reviews.test$neg,1,0)
mean (ifelse (reviews.test$polarity != pred,1,0))

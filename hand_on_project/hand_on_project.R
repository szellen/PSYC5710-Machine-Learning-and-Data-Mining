#install.packages("tm")  
#install.packages("SnowballC")

setwd("/Users/szell/move on/2020Spring/PSYC5710/PSYC5710-Machine-Learning-and-Data-Mining/hand_on_project")

load("./Reuters.Rdata")

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

removeNonWords <- function(x) gsub("[^a-zA-Z]+", " ", x)

library(tm)
corpus <- Corpus(DataframeSource(reuters.train))
corpus <- tm_map(corpus, removeURL)
corpus <- tm_map(corpus, removeNonWords)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus, stemDocument)

head(corpus2)

# Question 1: 18119 terms
dtm.reuters <- DocumentTermMatrix(corpus2)
dtm.reuters

#Question 2: 1645
#remove sparse term
dtm.reuters <- removeSparseTerms(dtm.reuters, 0.98)
dtm.reuters

#Question 3£º most frequent words - said, year, will
dtm.data <- as.data.frame(as.matrix(dtm.reuters))
head(dtm.data)

freq.dtm <- sort(colSums(dtm.data),decreasing=TRUE)
freq.data <- data.frame(word = names(freq.dtm),freq=freq.dtm)

head(freq.data)

#Question4: sentiment analysis scoring tweets = 1 (positive)
#install.packages("SentimentAnalysis")  
#install.packages("SnowballC")
#import laxicon
liu.pos.words <- scan("./positive-words.txt", 
                      what = "character", 
                      comment.char = ";", encoding = "UTF-8")
liu.neg.words <- scan("./negative-words.txt", 
                      what = "character", 
                      comment.char = ";", encoding = "UTF-8")

# Generating the scores for the words:
liu.pos.scores <- rep(1, length(liu.pos.words))
liu.neg.scores <- rep(-1, length(liu.neg.words))

liu.lexicon <- c(liu.pos.words, liu.neg.words)
liu.scores <- c(liu.pos.scores, liu.neg.scores)

#scoring function
scoring.texts <- function(text, pos, neg) {require(plyr)
  require(stringr)
  scores <- ldply(text, function(text, pos, neg) {
    words0 <- str_split(text, '\\s+')
    words <- unlist(words0)
    positive <- sum(!is.na(match(words, pos)))
    negative <- sum(!is.na(match(words, neg)))
    score <- positive - negative
    all <- data.frame(score, positive, negative)
    return(all)
  }, pos, neg)
  scores.df = data.frame(scores, text=text)
  return(scores.df)
}

#scoring tweets = 1 (positive)
head(reuters.train)


for(i in 1:length(corpus2)){
  reuters.train$text.tm[i] <- strwrap(corpus2[[i]], 10000)
}

reuters.train$text.tm[1]

SentimentLiuHu <- scoring.texts(text = reuters.train$text.tm, 
                                pos = liu.pos.words, neg = liu.neg.words)
head(SentimentLiuHu)
median(SentimentLiuHu$score)

#############################################################################
################### Part 2 ##################################################
dtm3 <- removeSparseTerms(dtm.reuters, 0.75)
dtm3

dtm.reuters2 <- as.data.frame(as.matrix(dtm3))
head(dtm.reuters2)

# Question 5: max degree centrality is 'analyst'   
library(tm)
cor.reuters <- cor_auto(dtm.reuters2,forcePD=TRUE)
library(qgraph)
centrality <- centrality(cor.reuters) # calculate the degree centrality
centrality

max(centrality$OutDegree)

# Question 6: latent topic = 12
library(EGAnet)
ega.reuters.ggm <- EGA(data=cor.reuters, n = nrow(dtm.reuters2), model = "glasso")

ega.reuters.ggm
# 1£ºmarket 
# 2: industry
# 3£ºbasic description terms
# 4£ºtrade and stock
# 5£ºprofit,sale and market
# 6£ºnews sourse
# 7£ºbasic verbs
# 8£ºleading team
# 9£ºbusiness operation
# 10£ºUnited States operation
# 11£ºbasic numbers
# 12£ºfinance



# Question 7: latent topic = 6
library(EGAnet)
ega.reuters.ggm <- EGA(data=cor.reuters, n = nrow(dtm.reuters2), model = "TMFG")

ega.reuters.ggm 
# 1£ºgovernment and politic 
# 2: market and finance
# 3: united stated related
# 4£ºtrade and stock
# 5£ºbusiness
# 6£ºinternational news



# Question 8: dynEGA latent topic = 6
load("./Reuters.Rdata")
head(dtm.reuters2)
dtm.reuters2$id <- reuters.train$Author

dyn.reuters <- dynEGA(data = dtm.reuters2, n.embed = 5, tau = 1,
                    delta = 1, id = 99, use.derivatives = 1,
                    level = "population", model = "glasso", cor="spearman")

plot(dyn.reuters)
dyn.reuters
# 1£ºgovernment decision
# 2: market and finance
# 3: government and politic 
# 4£ºtrade, stock, market
# 5£ºunited states
# 6£ºnews source

#Question 9 : not in the top 10 highest mean happy emotion: Kevin Drawbaugh 
#emotional lexicon 
load("./emotions.texts.Rdata")
emotions <- read.csv("emotions.csv", header = TRUE, stringsAsFactors = FALSE)
head(emotions)


tweetEmotion <- emotions.texts(text=reuters.train$text.tm, emotions=emotions)
tweetEmotion$author <- reuters.train$Author

tweetEmotion$Zhappy <- scale(tweetEmotion$HAPPY, center = TRUE, scale=TRUE) # transform to standardized scores

happy_sum <- aggregate(tweetEmotion$Zhappy, by=list(author=tweetEmotion$author), FUN=sum)
order(happy_sum$V1)
# Alan Crosby 2rd
# Kevin Drawbaugh 
# Darren Schuettler 1st
# Sarah Davison 7th 

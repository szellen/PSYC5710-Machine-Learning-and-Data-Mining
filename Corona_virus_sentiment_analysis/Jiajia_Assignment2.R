load("./data.corona.RData")
head(data.corona)

#detect English post
#install.packages("cld3")
library(cld3)

data.corona$Language <- detect_language(data.corona)

data.en.noretweet <- data.corona[which(data.corona$Language=="en" & data.corona$isRetweet==FALSE), ]


#pre-process the tweets
#install.packages("tm")  
library(tm)

#data.en.noretweet$text <- iconv(data.en.noretweet$text, 'UTF-8', 'ASCII') ---this line is causing the differece

corpus <- Corpus(DataframeSource(data.en.noretweet))
removeURL <- function(x) gsub("http[ˆ[:space:]]*", "", x) #remove URL

corpus <- tm_map(corpus, removeURL)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

for(i in 1:length(corpus)){
  data.en.noretweet$text.tm[i] <- strwrap(corpus[[i]], 10000)
}

data.en.noretweet$text.tm[1]

#convert to DTM
dtm.corona <- DocumentTermMatrix(corpus)
dtm.corona
data.en.noretweet.dtm2 <- removeSparseTerms(dtm.corona, 0.990) #remove sparse term 
data.en.noretweet.dtm2

#find most frequent term
dtm.data <- as.data.frame(as.matrix(data.en.noretweet.dtm2))
head(dtm.data)
freq.dtm <- sort(colSums(dtm.data),decreasing=TRUE)
freq.dtm

#sentiment analysis to compute sentiment scores
#install.packages("SentimentAnalysis")  
#install.packages("SnowballC")
library (SentimentAnalysis)
sentiment.corona <- analyzeSentiment(data.en.noretweet$text.tm)
head(sentiment.corona)

sentiments <- data.frame(SentimentLM=sentiment.corona$SentimentLM, SentimentQDAP =sentiment.corona$SentimentQDAP,
                         Region =data.en.noretweet$Region, WordCount =sentiment.corona$WordCount,
                         NegativityGI=sentiment.corona$NegativityGI)
head(sentiments)

#means of the SentimentLM score
#install.packages("ggpubr")
library(ggpubr)
compare_means(SentimentLM~Region, data = sentiments, method = "wilcox.test")

# plot the SentimentLM score -Box Plot
comparisons.list <- list(c("Charlottesville", "Hong Kong"),
                         c("Charlottesville", "Wuhan"),
                         c("Hong Kong", "Wuhan"))

ggboxplot(sentiments, x="Region", y="SentimentLM",
          color = "Region", palette ="jco",
          add = c("jitter","violin","mean"),add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif") # Add pairwise comparisons p-value

# plot the SentimentLM score -Line Plot
ggerrorplot(sentiments, x = "Region", y = "SentimentLM", 
            desc_stat = "mean_se",
            color = "Region", palette ="jco",
            error.plot = "errorbar",            # Change error plot type
            add = "mean",add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif")

############################Sentiment Liu Hu Score ######################
#########################################################################
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
  #scoring tweets
SentimentLiuHu <- scoring.texts(text = data.en.noretweet$text.tm, 
                              pos = liu.pos.words, neg = liu.neg.words)
head(SentimentLiuHu)

  #compare LiuHu Laxicon with sentimentAnalysis package
plot(SentimentLiuHu$score, sentiments$SentimentLM)

  #compare mean of LiuHu score
SentimentLiuHu$Region <- sentiments$Region
library(ggpubr)
compare_means(score~Region, data = SentimentLiuHu, method = "wilcox.test")

#plotting SentimentLiuHu plot
comparisons.list <- list(c("Charlottesville", "Hong Kong"),
                         c("Charlottesville", "Wuhan"),
                         c("Hong Kong", "Wuhan"))

ggboxplot(SentimentLiuHu, x="Region", y="score",
          color = "Region", palette ="jco",
          add = c("jitter","violin","mean"),add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif") # Add pairwise comparisons p-value

# plot the SentimentLM score -Line Plot
ggerrorplot(SentimentLiuHu, x = "Region", y = "score", 
            desc_stat = "mean_se",
            color = "Region", palette ="jco",
            error.plot = "errorbar",            # Change error plot type
            add = "mean",add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif")


############################### Emotion lexicon ###############################
###############################################################################
load("./emotions.texts.Rdata")
emotions <- read.csv("emotions.csv", header = TRUE, stringsAsFactors = FALSE)
head(emotions)

#use tweets with more tha  five words
sentiment.corona$text.tm <- data.en.noretweet$text.tm
sentiment.corona$Region <- data.en.noretweet$Region

sentiment.corona.long <- sentiment.corona[which(sentiment.corona$WordCount>5), ]
colnames(sentiment.corona.long)

tweetEmotion <- emotions.texts(text=sentiment.corona.long$text.tm, emotions=emotions)
head(tweetEmotion[,1:8])

tweetEmotion$Region <- sentiment.corona.long$Region

colnames(tweetEmotion)

#compare mean
library(ggpubr)
compare_means(SAD~Region, data = tweetEmotion, method = "wilcox.test")

#plotting
comparisons.list <- list(c("Charlottesville", "Hong Kong"),
                         c("Charlottesville", "Wuhan"),
                         c("Hong Kong", "Wuhan"))

ggboxplot(tweetEmotion, x="Region", y="SAD",
          color = "Region", palette ="jco",
          add = c("jitter","violin","mean"),add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif") # Add pairwise comparisons p-value

# plot the SentimentLM score -Line Plot
ggerrorplot(tweetEmotion, x = "Region", y = "SAD", 
            desc_stat = "mean_se",
            color = "Region", palette ="jco",
            error.plot = "errorbar",            # Change error plot type
            add = "mean",add.params = list())+ 
  stat_compare_means(comparisons = comparisons.list, label = "p.signif")


############################Using Rasch Model######################################3
####################################################################################
#install.packages("eRm")
library(eRm)

# Transforming the scores, so that > 2 --> 1;

tweetEmotion.Cville <- sentiment.corona[which(tweetEmotion$Region=="Charlottesville"), ]
tweetEmotion.HK <- sentiment.corona[which(tweetEmotion$Region=="Hong Kong"), ]
tweetEmotion.Wuhan <- sentiment.corona[which(tweetEmotion$Region=="Wuhan"), ]

emotions.rasch <- as.data.frame(apply(tweetEmotion.HK[,1:8], 2, 
                                            function(x) ifelse(x>2, 2, 1)))
# Identifying NAs > 6:
emotions.rasch.na <- apply(emotions.rasch, 1, 
                                 function(x) sum(is.na(x))>6)
# Using only cases with NAs <6:
res.rm <- RM(emotions.rasch[!emotions.rasch.na,])
head(res.rm)

plotPImap(res.rm, sorted = TRUE)



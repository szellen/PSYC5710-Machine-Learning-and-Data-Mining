install.packages("tm")  
install.packages("SnowballC")
install.packages("ggplot2")  
install.packages("qgraph")
install.packages("plotly")  
install.packages("dplyr")
install.packages("wordcloud")

#import texting data
lyrics_beatles <- read.csv("./lyrics_beatles.csv", 
                           header = TRUE, stringsAsFactors = FALSE)


str(lyrics_beatles)
lyrics_beatles[13,1] #song name
lyrics_beatles[13,2] #sone writer
lyrics_beatles[13,3] #sone writer

#transform to text document collection
library(tm)
corpus <- Corpus(VectorSource(lyrics_beatles[,3]))
corpus

corpus[[1]]$content

#to lower
lyrics_beatles[167,3]
tolower(lyrics_beatles[167,3])
corpus<- tm_map(corpus, tolower)
corpus

# Applying more transformations to the corpus object (removing ponctuation and numbers):
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus[[1]]

#remove stopword and decrease sparity
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm.beatles <- DocumentTermMatrix(corpus)

dtm.beatles <- removeSparseTerms(dtm.beatles, 0.90)
dtm.beatles

#find association/correlation
findAssocs(dtm.beatles, "love", 0.80)

#transform the document term matrix into a dataframe
dtm.data <- as.data.frame(as.matrix(dtm.beatles))
head(dtm.data)

#plotting 
freq.dtm <- sort(colSums(dtm.data),decreasing=TRUE)
freq.data <- data.frame(word = names(freq.dtm),freq=freq.dtm)
library(ggplot2)
freq.plot <- ggplot(freq.data, aes(reorder(word, freq), freq)) + 
  geom_col() + xlab(NULL) + coord_flip() + 
  ylab("Frequency") + 
  theme(text = element_text(size = 15))
freq.plot

freq.dtm

#plot dynamic heatmap
library(qgraph)
library(plotly)
library(dplyr)
cor.terms <- cor_auto(dtm.data)
a <- list(showticklabels = TRUE, tickangle = -45)
plot.cor <- plot_ly(x = colnames(cor.terms), y = colnames(cor.terms),
                    z = cor.terms, type = "heatmap") %>%
  layout(xaxis = a,  showlegend = FALSE, 
         margin = list(l=100,b=100,r=100,u=100))
plot.cor

#plot word cloud
library(wordcloud)
set.seed(1234)
wordcloud(words = freq.data$word, freq = freq.data$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

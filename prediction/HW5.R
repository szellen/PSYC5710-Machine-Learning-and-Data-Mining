#setwd("/Users/szell/move on/2020Spring/PSYC5710/PSYC5710-Machine-Learning-and-Data-Mining/prediction")

load("./scores.sentiment.trump.RData")


## QUestion 1: significantly related: SentimentQDAP
lm1<- lm(retweetCount~., data = scores.sentiment.trump)
summary(lm1)

## Question 2: 0.8717
summary(lm1)$adj.r.squared


## Question 3: sentimentQDAP impacts more negatively
lm2<- lm(retweetCount~SentimentQDAP + favoriteCount, data = scores.sentiment.trump)
summary(lm2)

## Question 4: 6 predictors: Topic1 + Topic2 + Topic4 + Topic5 + SentimentQDAP + favoriteCount
library(MASS)
step <- stepAIC(lm1, direction="backward")
step$anova

## Question 5: h(Topic2- -0.071) x h(0.363636-SentimentQDAP) has highest positive coefficient
##h(0.168-Topic4); -1.724055e+03
##h(Topic2- -0.071) x h(0.363636-SentimentQDAP); 5.142497e+03
##h(Topic2- -0.071) * h(175776-favoriteCount)  -1.954692e-02
scores.sentiment.trump2 <- na.omit(scores.sentiment.trump)

##install.packages("earth")
library(earth)
fit.mars <- earth(retweetCount~., scores.sentiment.trump2, ncross=10, nfold=2, 
                                keepxy=TRUE, degree = 2, varmod.method = "earth")
fit.mars$coefficients

## Question 6: R-Squared is 0.9078
fit.mars$rsq

## Question 7: the number of term increase
## allowing up to 4 predictor to interact compared with only 2 in the previous question --> more combination
fit.mars2 <- earth(retweetCount~., scores.sentiment.trump2, ncross=10, nfold=2, 
                  keepxy=TRUE, degree = 4, varmod.method = "earth")
fit.mars2$coefficients

## Question 8: R-Squared is 0.9174
fit.mars2$rsq

## Question 9: 31326.9
new.data <- data.frame("Topic1" = 0.021, "Topic2" = -0.28, "Topic3" = 1.342, "Topic4" = 0.168, "Topic5" = -0.3, "Topic6" = -1.159, "WordCount"=9,"SentimentGI" = 0.1111111,
                       "SentimentLM" = 0,"SentimentQDAP" = -0.1111111,"favoriteCount" = 127735,"Hour" = 0)
new.data

weights <- model.matrix(fit.mars2, new.data)
dim(weights)
dim(fit.mars2$coefficients)

prediction<- weights %*% fit.mars2$coefficients
prediction

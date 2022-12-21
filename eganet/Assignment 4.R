## install packages
#EGAnet:
#install.packages('devtools')
library(devtools)

#install_github("hfgolino/EGAnet")
#NetworkToolbox
#install_github("AlexChristensen/NetworkToolbox")
setwd("/Users/szell/move on/2020Spring/PSYC5710/PSYC5710-Machine-Learning-and-Data-Mining/eganet")

#load data
load("./data.trump.Rdata")
head(data.trump)

## 1: number of words/terms: 102
ncol(data.trump)


library(qgraph)
data.trump <- data.trump[,0:102]
head(data.trump)
cor.trump <- cor_auto(data.trump,forcePD=TRUE)  

cor.trump
##3 : highest degree centrality: "vote"
centrality <- centrality(cor.trump)
max(centrality$InDegree)
centrality$InDegree

#4: highest betweeness: "vote"
max(centrality$Betweenness)
centralityPlot(cor.trump,include =c("Betweenness"))

#5: highest strength centrality: "tonight"
max(centrality$InExpectedInfluence)
centrality$InExpectedInfluence

#7: number of latent factors using EGA function: 15
library(EGAnet)
ega.trump.ggm <- EGA(cor.trump, n = nrow(data.trump), model = "glasso")
ega.trump.ggm
#8: interpret
# 1£ºrelated to international policy
# 2£ºrealted to orders and operation
# 3£ºcampaign
# 4£ºharassing to president
# 5£ºmilitary operations
# 6£ºnorth korea related topic
# 7£ºnaegativity toward Michael Cohen
# 8£ºnegativity toward media fake news
# 9£ºabout foxnew
# 10£ºUnited States
# 11£ºAndrew Mccabe
# 12£ºtopics relatd to america and venezuela
# 13£ºpositivity toward job
# 14£ºpositivity about the country 
# 15£ºabout crime

#9: number of latent factors using EGA function: 5
ega.trump.tmfg <- EGA(cor.trump, n = nrow(data.trump), model = "TMFG")

#10: interpret 
ega.trump.tmfg
# 1£ºpolitics terms, 
# 2£ºnegative terms 
# 3£ºpositive and promising words
# 4£ºwords that has a very strong emtional component
# 5£ºthese are less related terms and are netural in emotion


#12: Pearson correlations possitive pair: 3 & 4 topics

ega.trump.tmfg$wc
head(data.trump)

scores.trump <- net.scores(data.trump[-c(103)],
                           A = ega.trump.tmfg$network,
                           wc =ega.trump.tmfg$wc,
                           type = "sumscore")
scores.trump2 <- as.data.frame(scores.trump$std.scores)

colnames(scores.trump2) <- paste0("Topic",colnames(scores.trump2))
head(scores.trump2)

cor(scores.trump2, use="pairwise.complete.obs", method="pearson")

#13: TMFG estimation is stable: 
boot.ega <- bootEGA(na.omit(data.trump), type = "parametric", n = 100, model = "TMFG") # error with dimensions

#14: 17 latent topic
load("./data.trump.Rdata")
dyn.trump <- dynEGA(data = data.trump, n.embed = 5, tau = 1,
                     delta = 1, id = 103, use.derivatives = 1,
                     level = "population", model = "glasso", cor="spearman")

plot(dyn.trump)

dyn.trump

# 1£ºinternational policy
# 2£ºelections and campaign
# 3£ºreporting record and rate
# 4£ºsentences in elections and campaign
# 5£ºrelated to build the wall
# 6£ºMichael Cohen
# 7£ºKim Jong
# 8£ºharrasing president
# 9£ºmedia fake news
# 10£ºjobs
# 11£ºAndrew Mccabe
# 12£ºsomething about the lie
# 13£ºcommitte (not sure)
# 14£ºUnited States
# 15£ºvenezuela human right 
# 16: Trump new book
# 17£º grestest country in the history

#15: 17 latent topic
dyn.trump <- dynEGA(data = data.trump, n.embed = 5, tau = 1,
                    delta = 1, id = 103, use.derivatives = 2,
                    level = "population", model = "glasso", cor="spearman")

plot(dyn.trump)
dyn.trump
# 1£ºAmerican policy
# 2: millitary, crime, and security
# 3£ºnews and humanity
# 4£ºjob
# 5£ºtrade?
# 6£ºsenate and committe
# 7£ºfakenews
# 8£ºnorth korea
# 9£ºunited states
# 10£ºgreatest country in history
# 11£ºKim jong
# 12£ºvietnam
# 13£ºnot sure how build and fall related
# 14£ºMichael Cohen
# 15£ºTrump new book
# 16: Harassing president
# 17£ºDemocrat


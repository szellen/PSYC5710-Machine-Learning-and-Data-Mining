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
# 1��related to international policy
# 2��realted to orders and operation
# 3��campaign
# 4��harassing to president
# 5��military operations
# 6��north korea related topic
# 7��naegativity toward Michael Cohen
# 8��negativity toward media fake news
# 9��about foxnew
# 10��United States
# 11��Andrew Mccabe
# 12��topics relatd to america and venezuela
# 13��positivity toward job
# 14��positivity about the country 
# 15��about crime

#9: number of latent factors using EGA function: 5
ega.trump.tmfg <- EGA(cor.trump, n = nrow(data.trump), model = "TMFG")

#10: interpret 
ega.trump.tmfg
# 1��politics terms, 
# 2��negative terms 
# 3��positive and promising words
# 4��words that has a very strong emtional component
# 5��these are less related terms and are netural in emotion


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

# 1��international policy
# 2��elections and campaign
# 3��reporting record and rate
# 4��sentences in elections and campaign
# 5��related to build the wall
# 6��Michael Cohen
# 7��Kim Jong
# 8��harrasing president
# 9��media fake news
# 10��jobs
# 11��Andrew Mccabe
# 12��something about the lie
# 13��committe (not sure)
# 14��United States
# 15��venezuela human right 
# 16: Trump new book
# 17�� grestest country in the history

#15: 17 latent topic
dyn.trump <- dynEGA(data = data.trump, n.embed = 5, tau = 1,
                    delta = 1, id = 103, use.derivatives = 2,
                    level = "population", model = "glasso", cor="spearman")

plot(dyn.trump)
dyn.trump
# 1��American policy
# 2: millitary, crime, and security
# 3��news and humanity
# 4��job
# 5��trade?
# 6��senate and committe
# 7��fakenews
# 8��north korea
# 9��united states
# 10��greatest country in history
# 11��Kim jong
# 12��vietnam
# 13��not sure how build and fall related
# 14��Michael Cohen
# 15��Trump new book
# 16: Harassing president
# 17��Democrat


## This script takes the global bibliography then splits it into separate dataframes for each crop that are compiled into
## a large list. The corresponding workspace is 'Crops_split_workspace.RData'

### Reset java parameters, load libraries, load tags, and load custom functions.
setwd("~/Documents/P_grains/GITHUB/topic_model_article")
options(java.parameters="-Xmx3g")
library(reshape2)
library(plyr)
library(topicmodels)
source("custom_functions.R")
source("tags2.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("P_grains_relevant.csv", na.strings="")
d <- d[!is.na(d$Abstract.Note),]

all <- rep( list(data.frame()), 6) 
names(all) <- crop.names

for(j in 1:length(all)){
  all[[j]] <- subit(data=d, all.tags[[j]])
}

### run topic models on all dataframes in list ####
all_topics <- llply(.data=all, .fun=nouns_adj_only_n_grams_topics, k=3,seed=2000)

#### Generate most likely terms from all topic models ####

all_terms <- rep( list(list()), 6 ) 
names(all_terms) <- names(all_topics)

for(i in 1:length(all_terms)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}

### assign papers and keywords to topics for all crops and all models

topic_assign_all <- rep(list(), length(all_topics))
topic_assign_terms <- rep(list(), length(all_topics))
model.names <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

for(i in names(all_topics)){ 
    for(j in model.names){
        post <- (posterior(all_topics[[i]][[j]]))
        topic_assign_all[[i]][[j]] <- data.frame(cbind(unlist(post[["topics"]]), 
                                                       c(as.numeric(rownames(post[["topics"]]))), 
                                                       unlist(topics(all_topics[[i]][[j]]))))
        colnames(topic_assign_all[[i]][[j]]) <- c("top_1_prob","top_2_prob","top_3_prob","pub_number","Topic")
        topic_assign_all[[i]][[j]] <- melt(topic_assign_all[[i]][[j]],
                                           id.vars = .(pub_number, Topic),
                                           measure.vars = .(top_1_prob, top_2_prob, top_3_prob),
                                           value.name = "article.posterior.probability")
        topic_assign_sel <- c()
        for (k in 1:3){
            topic_assign_sel <- rbind(topic_assign_sel,
                                      topic_assign_all[[i]][[j]][topic_assign_all[[i]][[j]]$Topic == k &
                                                                     topic_assign_all[[i]][[j]]$variable ==
                                                                         paste("top_", k, "_prob", sep = ""),
                                                                 c(1:2,4)])
        }
        topic_assign_all[[i]][[j]] <- topic_assign_sel
        topic_assign_terms[[i]][[j]] <- melt(unlist(post$terms))
        colnames(topic_assign_terms[[i]][[j]]) <- c("Topic", "Term", "term.posterior.probability")
    }}

topic_assign_pub <- rep(list(), length(all_topics))

for (i in names(all)){
    for (j in model.names){
        all[[i]]$pub_number <- row.names(all[[i]])  
        topic_assign_pub[[i]][[j]] <- merge(all[[i]],
                                            topic_assign_all[[i]][[j]], 
                                            by="pub_number", sort=F)
    }}

### Generate csv files that contain top papers assigned to each topic
### (prob >= .7 if greater than 5 articles assigned) from the Gibbs model

articles <- c()

file.name <- "by-crop"
for(i in names(topic_assign_pub)){
    articles <- topic_assign_pub[[i]][["Gibbs"]][,c("Topic",
                                                    "Author", "Publication.Year", "Title", "Publication.Title",
                                                    "article.posterior.probability", "Abstract.Note")]
    articles <- articles[with(articles,
                              order(Topic, -article.posterior.probability)),]

    articles2 <- c()

    for (j in 1:3){   
        if (dim(articles[articles$Topic == j,]
                [articles[articles$Topic == j,]
                 $article.posterior.probability >= .7,])[1] < 5)
            articles2 <-  rbind(articles2,
                                head(articles[articles$Topic == j,], n=5))
        else {
            articles2 <- rbind(articles2, articles[articles$Topic == j,]
                               [articles[articles$Topic == j,]
                                $article.posterior.probability >= .7,])
        }}
                           
    articles3 <- c()

    for (j in 1:dim(articles2)[1]){
        articles3 <- rbind(articles3,
                           data.frame(Topic=articles2$Topic[j],
                                      Article=paste(gsub(";", ",", articles2$Author[j]),
                                          " (", articles2$Publication.Year[j], ") ",
                                          articles2$Title[j], ". ", articles2$Publication.Title[j], sep=""),
                                      Probability=articles2$article.posterior.probability[j],
                                      Abstract=articles2$Abstract.Note[j]))
    }

    articles <- articles3
    rm(articles2,articles3)
    articles$Probability <- round(articles$Probability, digits = 4)
    write.csv(articles, row.names=F, file=paste(getwd(),"/topic-articles/","articles-", file.name, "-",
                                         i, ".csv",sep=""))
}

### Generate csv files that contain most likely terms for each topic
### (posterior probability >= .01) from the Gibbs model

topic.terms <- c()
topic.terms.all <- rep(list(), length(all_topics))
for(i in names(topic_assign_terms)){
    topic.terms <- c()
    for(j in 1:3){
        topic.terms <- rbind(topic.terms, topic_assign_terms[[i]][["Gibbs"]][topic_assign_terms[[i]][["Gibbs"]]
                                                                             $term.posterior.probability >= .01 &
                                                                                 topic_assign_terms[[i]][["Gibbs"]]
                                                                             $Topic == j,])
        topic.terms <- topic.terms[with(topic.terms,
                                        order(Topic, -term.posterior.probability)),]
        topic.terms$term.posterior.probability <- round(topic.terms$term.posterior.probability, digits = 4)
        write.csv(topic.terms, row.names=F, file=paste(getwd(),"/topic-terms/","terms-", file.name, "-",
                                                i, ".csv",sep=""))
        topic.terms.all[[i]] <- topic.terms
    }}

detach(package:reshape2, unload=T)
detach(package:topicmodels, unload=T)

### Generate graphs from the graphs.R script and save to WD.

dx.topic.time.all <- c()
dx.topic.count.all <- c()

for(i in 1:length(topic_assign_pub)){
    dx <- topic_assign_pub[[i]][["Gibbs"]][,c("Topic", "Publication.Title", "Publication.Year")]
    dx.tag <- names(topic_assign_pub)[[i]]
    dx$Decade <- round_any(dx$Publication.Year,10, f=floor)
    for (j in 1:3){
        dx$Topic[dx$Topic == j] <- paste(
                     paste("TOPIC", j, sep = " "),
                     topic.terms.all[[i]]$Term[topic.terms.all[[i]]$Topic == j][1],
                     topic.terms.all[[i]]$Term[topic.terms.all[[i]]$Topic == j][2],
                     topic.terms.all[[i]]$Term[topic.terms.all[[i]]$Topic == j][3],
                     topic.terms.all[[i]]$Term[topic.terms.all[[i]]$Topic == j][4],
                     topic.terms.all[[i]]$Term[topic.terms.all[[i]]$Topic == j][5],
                     sep = "\n")
    }
        source("graphs.R")
}
    



source("graphs-overall.R")
source("graphs2.R")

### Save workspace.

save(all, all_terms, all_topics, topic_assign_all, topic_assign_pub,
     topic_assign_terms, topic.terms.all, file.name, d,
     file="crops_split_workspace.RData")

## This script takes the global bibliography then splits it into separate dataframes for each crop that are compiled into
## a large list. The corresponding workspace is 'Crops_split_workspace.RData'

### Reset java parameters, load libraries, load tags, and load custom functions.

options(java.parameters="-Xmx3g")
setwd("~/Documents/P_grains/GITHUB/topic_model_article/")
library(plyr)
library(reshape2)
source("custom_functions.R")
source("tags2.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("P_grain_biblio_paper_database.csv")

all <- rep( list(data.frame()), 6) 
names(all) <- crop.names

for(j in 1:length(all)){
  all[[j]] <- subit(data=d, all.tags[[j]])
}

### run topic models on all dataframes in list ####
detach("package:ggplot2", unload=TRUE)
all_topics <- llply(.data=all, .fun=nouns_adj_only_n_grams_topics, k=3,seed=2000)

#### Generate most likely terms from all topic models ####

all_terms <- rep( list(list()), 6 ) 
names(all_terms) <- names(all_topics)

for(i in 1:length(all_terms)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}


#lapply(top.terms, write, file=paste(getwd(),"/figures/",dx.tag,"_topic_terms.txt",sep=""), append=T, ncolumns=1000)

### assign papers to topics for all crops and all models 

topic_assign_all <- rep(list(list()), 6)
names(topic_assign_all) <- crop.names


for(i in 1:length(all_topics)){ 
for(j in 1:4){
post <- (posterior(all_topics[[i]][[j]]))
topic_assign_all[[i]][[j]] <- data.frame(cbind(unlist(post[[2]]), 
                                 c(as.numeric(rownames(post[[2]]))), 
                                 unlist(topics(all_topics[[i]][[j]]))))
colnames(topic_assign_all[[i]][[j]]) <- c("top_1_prob","top_2_prob","top_3_prob","pub_number","topic_assign")
}
names(topic_assign_all[[i]]) <- names(all_topics[[i]])
}


for(i in 1:length(all)){
  all[[i]]$pub_number <- row.names(all[[i]])  
  all[[i]] <- merge(all[[i]],topic_assign_all[[i]][[1]][,c("pub_number","topic_assign")], 
                    by="pub_number", sort=F)
  names(all[[i]])[names(all[[i]])=="topic_assign"] <- "VEM_assign"}

for(i in 1:length(all)){
for(j in 2:4){  
  all[[i]]$VEM_fixed_assign <- merge(all[[i]],topic_assign_all[[i]][[j]][,c("pub_number","topic_assign")], 
                                     by="pub_number", sort=F)[,"topic_assign"]  
  all[[i]]$Gibbs_assign <- merge(all[[i]],topic_assign_all[[i]][[j]][,c("pub_number","topic_assign")], 
                                 by="pub_number", sort=F)[,"topic_assign"]
  all[[i]]$CTM_assign <- merge(all[[i]],topic_assign_all[[i]][[j]][,c("pub_number","topic_assign")], 
                               by="pub_number", sort=F)[,"topic_assign"]
}}


### Generate csv files that contain most likely terms for each topic


for(i in 1:length(all_terms)){
  write.csv(makePaddedDataFrame(all_terms[[i]][[3]]), 
            file=paste(getwd(),"/topic_terms/",names(all_terms)[[i]],"_top_terms.csv",sep=""))
}

### Generate graphs from the graphs.R script and save to WD. NOTE: graphs2.R generates a better histogram over time graph
### but cannot be run through this for loop. Simply load the workspace, then run graphs2.R

for(i in 1:length(all)){
  dx <- all[[i]]
  dx.tag <- names(all)[[i]]
  dx$topic.assign <- dx$Gibbs_assign
  source("graphs.R")
}

### Save workspace.

save(all, all_terms, all_topics, topic_assign_all, d, file="crops_split_workspace.RData")



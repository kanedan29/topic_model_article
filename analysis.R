setwd("~/Documents/P_grains/GITHUB/topic_model_article/")
library(plyr)
source("custom_functions.R")
source("tags.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("Zotero_db_dups_removed.csv")
all <- rep( list(data.frame()), 7 ) 
names(all) <- crop.names

for(j in 1:length(all)){
  all[[j]] <- subit(data=d, all.tags[[j]])
}


### run topic models on all dataframes in list ####

all_topics <- llply(.data=all, .fun=n_grams_topics, k=3,seed=2000)

#### Generate most likely terms from all topic models ####

all_terms <- rep( list(list()), 7 ) 
names(all_terms) <- names(all_topics)

for(i in 1:length(all_terms)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}


#lapply(top.terms, write, file=paste(getwd(),"/figures/",dx.tag,"_topic_terms.txt",sep=""), append=T, ncolumns=1000)

### assign papers to topics for all crops and all models 

topic_assign_all <- rep(list(list()), 7 )
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







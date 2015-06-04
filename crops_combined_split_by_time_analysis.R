### Need to utilize cass.tags to remove cass papers

options(java.parameters="-Xmx3g")
setwd("~/Documents/P_grains/GITHUB/topic_model_article/")
library(plyr)
source("custom_functions.R")
source("tags2.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("P_grains_biblio_6_4_2015.csv")

d <- split(d,f=cut(d$Publication.Year, breaks=c(1900,1960,1990,2015)))
all <- d
names(all)<- c("1960","1990","2015")


### run topic models on all dataframes in list ####
detach("package:ggplot2", unload=TRUE)
all_topics <- llply(.data=all, .fun=nouns_adj_only_n_grams_topics, k=3,seed=2000)

#### Generate most likely terms from all topic models ####

all_terms <- list()


for(i in 1:length(all_topics)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}

names(all_terms) <- names(all_topics)

#lapply(top.terms, write, file=paste(getwd(),"/figures/",dx.tag,"_topic_terms.txt",sep=""), append=T, ncolumns=1000)

### assign papers to topics for all crops and all models 

topic_assign_all <- rep(list(list()), 11)

names(topic_assign_all) <- names(all_topics)


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


for(i in 1:length(all_terms)){
  write.csv(makePaddedDataFrame(all_terms[[i]][[3]]), 
            file=paste(getwd(),"/topic_terms/",names(all_terms)[[i]],"_top_terms.csv",sep=""))
}

for(i in 1:length(all)){
  dx <- all[[i]]
  dx.tag <- names(all)[[i]]
  dx$topic.assign <- dx$Gibbs_assign
  source("graphs.R")
}

save(all, all_terms, all_topics, topic_assign_all, d, file="crops_combined_split_time_workspace.RData")
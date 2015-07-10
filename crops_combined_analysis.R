## This script analyzes the global bibliography directly without subsetting it into separate libraries.

options(java.parameters="-Xmx3g")
library(plyr)
source("custom_functions.R")
source("tags2.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("P_grain_biblio_paper_database.csv")

all <- list(d)
names(all) <- "crops_combined"

### run topic models on all dataframes in list ####
detach("package:ggplot2", unload=TRUE)
all_topics <- llply(.data=all, .fun=nouns_adj_only_n_grams_topics, k=3,seed=2000)


#### Generate most likely terms from all topic models ####

all_terms <- list()


for(i in 1:length(all_topics)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}

names(all_terms) <- c("crops_combined")

#lapply(top.terms, write, file=paste(getwd(),"/figures/",dx.tag,"_topic_terms.txt",sep=""), append=T, ncolumns=1000)

### assign papers to topics for all crops and all models 

topic_assign_all <- rep(list(list()), 1)

names(topic_assign_all) <- "crops_combined"


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

## Generate CSVs that include the top terms for each topic.

for(i in 1:length(all_terms)){
  write.csv(makePaddedDataFrame(all_terms[[i]][[3]]), 
            file=paste(getwd(),"/topic_terms/",names(all_terms)[[i]],"_top_terms.csv",sep=""))
}

## Generate and save graphs to the WD using the graphs.R script.

for(i in 1:length(all)){
        dx <- all[[i]]
        dx.tag <- names(all)[[i]]
        dx$topic.assign <- dx$Gibbs_assign
        labels <- c()
        for (j in 1:3){
            labels <- rbind(labels,
                            paste(
                                paste("TOPIC", j, sep = " "),
                                all_terms[[i]][[3]][[j]][1],
                                all_terms[[i]][[3]][[j]][2],
                                all_terms[[i]][[3]][[j]][3],
                                all_terms[[i]][[3]][[j]][4],
                                all_terms[[i]][[3]][[j]][5],
                                sep = "\n"))
        }
#        source("graphs.R")
    }

## Save workspace to WD


save(all, all_terms, all_topics, topic_assign_all, d, file="crops_combined_workspace.RData")

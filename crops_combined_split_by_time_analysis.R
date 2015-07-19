### This script splits the global bibliography into separate libraries for three different time periods and then analyzes each

options(java.parameters="-Xmx3g")
library(plyr)
source("custom_functions.R")
source("tags2.R")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("P_grain_biblio_paper_database.csv")

d <- split(d,f=cut(d$Publication.Year, breaks=c(1930,1960,1990,2020)))
all <- d
names(all)<- c("1930-1959","1960-1989","1990-2015")

### run topic models on all dataframes in list ####
detach("package:ggplot2", unload=TRUE)
all_topics <- llply(.data=all, .fun=nouns_adj_only_n_grams_topics, k=3,seed=2000)

#### Generate most likely terms from all topic models ####

all_terms <- list()


for(i in 1:length(all_topics)){
  all_terms[[i]] <- llply(all_topics[[i]], .fun=function(x){terms(x=x,thresh=0.01)})
}

names(all_terms) <- names(all_topics)

### assign papers to topics for all crops and all models 

topic_assign_all <- rep(list(list()), 3)
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
  all[[i]]$pub_number <- c(1:length(all[[i]][[1]])) 
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

## Generate CSVs that include top terms for each topic

for(i in 1:length(all_terms)){
  write.csv(makePaddedDataFrame(all_terms[[i]][[3]]), 
            file=paste(getwd(),"/topic_terms/",names(all_terms)[[i]],"_top_terms.csv",sep=""))
}

## Generate and save graphs using graphs.R script


for(i in 1:length(all)){
    dx <- all[[i]]
    dx.tag <- names(all)[[i]]
    dx$Topic <- dx$Gibbs_assign
    dx$Decade <- round_any(dx$Publication.Year,10, f=floor)
    for (j in 1:3){
        dx$Topic[dx$Topic == j] <- paste(
                     paste("TOPIC", j, sep = " "),
                     all_terms[[i]][[3]][[j]][1],
                     all_terms[[i]][[3]][[j]][2],
                     all_terms[[i]][[3]][[j]][3],
                     all_terms[[i]][[3]][[j]][4],
                     all_terms[[i]][[3]][[j]][5],
                     sep = "\n")
    }
    source("graphs.R")
}

 ## Save workspace to WD.

save(all, all_terms, all_topics, topic_assign_all, d, file="crops_combined_split_time_workspace.RData")

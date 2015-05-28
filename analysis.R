setwd("~/Documents/P_grains/GITHUB/article/")
source("custom_functions.R")
library(plyr)

### generate tags for subsetting data ####

grain.tags <- list("perennial W/1 grain")
wheat.tags <- list("perennial W/1 wheat","perennial W/1 triticum")
pp.tags <- list('perennial W/1 pigeonpea','perennial W/1 "pigeon pea"','perennial W/1 cajanus','"long duration" W/1 pigeonpea',
                '"long duration" W/1 "pigeon pea"','"long duration" W/1 cajanus','ratoon\\* W/5 pigeonpea','ratoon\\* W/5 "pigeon pea"','ratoon\\* W/5 cajanus')
rice.tags <- list("perennial W/1 rice","perennial W/1 oryza","ratoon\\* W/5 rice","ratoon\\* W/5 oryza")
sorghum.tags <- list("perennial W/1 sorghum","ratoon\\* W/5 sorghum")
rye.tags <- list("perennial W/1 'rye' AND NOT 'ryegrass' AND NOT 'rye-grass' AND NOT 'rye grass'", "perennial W/1 secale")
cass.tags <- list("perennial W/1 cassava","perennial W/1 manihot")

all.tags <- lapply(ls(pattern="*tags"), function(x) get(x))
names(all.tags) <- c("cass","grain","pp","rice","rye","sorghum","wheat")

### read in data and recompile into a large list of dataframes for each crop ####

d <- read.csv("Zotero_db_dups_removed.csv")
all <- rep( list(data.frame()), 7 ) 
all[[1]] <- subit(data=d, all.tags[[1]])
all[[2]] <- subit(data=d, all.tags[[2]])
all[[3]] <- subit(data=d, all.tags[[3]])
all[[4]] <- subit(data=d, all.tags[[4]])
all[[5]] <- subit(data=d, all.tags[[5]])
all[[6]] <- subit(data=d, all.tags[[6]])
all[[7]] <- subit(data=d, all.tags[[7]])
names(all) <- c("cass","grain","pp","rice","rye","sorghum","wheat")

### run topic models on all dataframes in list ####

all_topics <- llply(.data=all, .fun=n_grams_topics, k=3,seed=2000)


#### Generate most likely terms from all topic models ####
all_terms <- rep( list(list()), 7 ) 
all_terms[[1]] <- llply(all_topics[[1]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[2]] <- llply(all_topics[[2]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[3]] <- llply(all_topics[[3]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[4]] <- llply(all_topics[[4]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[5]] <- llply(all_topics[[5]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[6]] <- llply(all_topics[[6]], .fun=function(x){terms(x=x,thresh=0.01)})
all_terms[[7]] <- llply(all_topics[[7]], .fun=function(x){terms(x=x,thresh=0.01)})
names(all_terms) <- names(all_topics)



#lapply(top.terms, write, file=paste(getwd(),"/figures/",dx.tag,"_topic_terms.txt",sep=""), append=T, ncolumns=1000)

perplexity(list(lda.model),newdata=dtm)
post <- (posterior(all_topics[[1]]))
terms <- data.frame(unlist(post[[1]]))
topic_assign <- data.frame(cbind(unlist(post[[2]]), 
                                 c(as.numeric(rownames(post[[2]]))), 
                                 unlist(topics(lda.model))))

colnames(topic_assign) <- c("top_1_prob","top_2_prob","top_3_prob","pub_number","topic_assign")
topic_assign$topic_assign <- as.integer(topic_assign$topic_assign)

dx.topic.count <- count(topic_assign, vars="topic_assign")



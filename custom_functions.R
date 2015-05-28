

### Topic modelng script written as an R function. "k" specifies the number of topics to be modeled. "data" specifies
#### the bibliographic dataset to be analyzed. To obtain model use this syntax: 'model <- pgrainstopics(.....)

n_grams_topics <- function(k,data,seed){  

### Topic model
library(tm)
library(SnowballC)
library(topicmodels)
library(slam)
library(RWeka)
myCorpus <- Corpus(VectorSource(data$Abstract.Note))

# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation & numbers
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove stopwords
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), stopwords("SMART"))
#stem words
dictionary <- c(matrix(DocumentTermMatrix(myCorpus)[[6]])[[2]])
myCorpus <- tm_map(myCorpus, stemDocument, language="eng")

stemCompletion_mod <- function(x,dictionary) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dictionary, type="shortest"),sep="", collapse=" ")))
}

#myCorpus <- tm_map(myCorpus, stemCompletion_mod, dictionary=dictionary)
## Sets the default number of threads to use (http://stackoverflow.com/questions/17703553/
#bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka)
options(mc.cores=1)
# Create bi-gram tokenizer function that can be applied to corpus to create DTM with bigrams in it
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = NGramTokenizer))  
######## LDA analysis #####
## remove infrequent terms - sparse terms approach
#dtm <-DocumentTermMatrix(myCorpus)
dtm = removeSparseTerms(dtm,0.99)
dtm = dtm[rowSums(as.matrix(dtm))>0,]

## remove infrequent terms - inverse document frequency approach
#term_tfidf <-
# + tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
#+ log2(nDocs(dtm)/col_sums(dtm > 0))
#dtm2 <- dtm[, term_tfidf >= 0.05]
#dtm2 <- dtm2[row_sums(dtm2) > 0,]
# LDA model of text corpus

topic.models <- 
  list(VEM = LDA(dtm, k = k, control = list(seed = seed)), 
       VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = seed)),
       Gibbs = LDA(dtm, k = k, method = "Gibbs",control = list(seed = seed, burnin = 1000,thin = 100, iter = 1000)),
       CTM = CTM(dtm, k = k, control = list(seed = seed, var = list(tol = 10^-4), em = list(tol = 10^-3))))

return(topic.models)

}


subit <- function(tags,data){
  library(plyr)
  b <- list()
  for(i in tags) { 
    b[[i]]<- data[grep(pattern=i,x=data$Manual.Tags,fixed=T),]}
  b <- ldply(b)[2:42]
  b <- unique.data.frame(b)
}


na.pad <- function(x,len){
  x[1:len]
}

makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(lapply(l,na.pad,len=maxlen),...)
}




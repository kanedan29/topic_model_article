### Topic modelng script written as an R function. "k" specifies the number of topics to be modeled. "data" specifies
#### the bibliographic dataset to be analyzed. To obtain model use this syntax: 'model <- pgrainstopics(.....)

###TO ADD - better duplicate removal, progress bar?

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
# remove crop terms
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), c("wheat","grain","triticum","rice","oryza","rye",
                                                               "secale","pigeonpea","pigeon pea","sorghum","cajanus","cassava","manihot"))
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

topics_simple <- function(k,data,seed){  
  
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
  # remove crop terms
  myCorpus <- tm_map(myCorpus, content_transformer(removeWords), c("wheat","grain","triticum","rice","oryza","rye",
                                                                "secale","pigeonpea","pigeon pea","sorghum","cajanus","cassava","manihot"))
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
  dtm <- DocumentTermMatrix(myCorpus) 
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

nouns_adj_only_topics <- function(k,data,seed){  
  
  ### Topic model
  library(openNLP)
  library(tm)
  library(SnowballC)
  library(topicmodels)
  library(slam)
  library(RWeka)
  myCorpus <- Corpus(VectorSource(data$Abstract.Note))
  
  ### Convert corpus to list of character vectors
  Corpus.str <- lapply(myCorpus, function(x){
    x <- as.String(x)  }  )
  
  ### Create empty list for corpus that only includes nouns and fill it using the
  ### nouns_only function
  Corpus.noun <- list()
  
  for(i in 1:length(Corpus.str)){
    Corpus.noun[[i]] <- nouns_adj_only(b=Corpus.str[[i]])
  }
  
  ### replace myCorpus with nouns only corpus
  myCorpus <- Corpus(VectorSource(Corpus.noun))
  # convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation & numbers
  myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
  # remove URLs
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  # remove crop terms
  myCorpus <- tm_map(myCorpus, content_transformer(removeWords), c("wheat","grain","triticum","rice","oryza","rye",
                                                                 "secale","pigeonpea","pigeon pea","sorghum","cajanus","cassava","manihot"))
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
  dtm <- DocumentTermMatrix(myCorpus)
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
  k=k
  seed=2000
  
  topic.models <- 
    list(VEM = LDA(dtm, k = k, control = list(seed = seed)), 
         VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = seed)),
         Gibbs = LDA(dtm, k = k, method = "Gibbs",control = list(seed = seed, burnin = 1000,thin = 100, iter = 1000)),
         CTM = CTM(dtm, k = k, control = list(seed = seed, var = list(tol = 10^-4), em = list(tol = 10^-3))))
  
  return(topic.models)
  }
  
nouns_adj_only_n_grams_topics <- function(k,data,seed){  
  
  ### Topic model
  library(openNLP)
  library(tm)
  library(SnowballC)
  library(topicmodels)
  library(slam)
  library(RWeka)
  myCorpus <- Corpus(VectorSource(data$full.text))
  
  ### Convert corpus to list of character vectors
  Corpus.str <- lapply(myCorpus, function(x){
    x <- as.String(x)  }  )
  
  ### Eliminate all papers that do not have abstracts
  Corpus.str <- Corpus.str[Corpus.str != ""]
  
  ### Create empty list for corpus that only includes nouns and fill it using the
  ### nouns_only function
  Corpus.noun <- list()
  
  for(i in 1:length(Corpus.str)){
    Corpus.noun[[i]] <- nouns_adj_only(b=Corpus.str[[i]])
  }
  
  ### replace myCorpus with nouns only corpus
  myCorpus <- Corpus(VectorSource(Corpus.noun))
  # convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation & numbers
  myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
  # remove URLs
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  # remove crop terms
  myCorpus <- tm_map(myCorpus, content_transformer(removeWords), c("wheat","grain","triticum","rice","oryza","rye",
                                                             "secale","pigeonpea","pigeon pea","sorghum","cajanus","cassava","manihot"))
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

nouns_only <- function(b){ 
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()  
  y1 <- annotate(b, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(b, pos_tag_annotator, y1)
  y2w <- subset(y2, type == "word")
  y2n <- subset(y2w, sapply(y2w$features, '[[', "POS") %in% c("NN","NN","NNP","NNPS"))
  r1<-b[y2n]
  r2 <- paste(r1, collapse = " ")
  return(r2)}

nouns_adj_only <- function(b){ 
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()  
  y1 <- annotate(b, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(b, pos_tag_annotator, y1)
  y2w <- subset(y2, type == "word")
  y2n <- subset(y2w, sapply(y2w$features, '[[', "POS") %in% c("NN","NN","NNP","NNPS","JJ","JJR","JJS"))
  r1<-b[y2n]
  r2 <- paste(r1, collapse = " ")
  return(r2)}

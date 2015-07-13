library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)
 
## Topic histogram per decade

years <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
                     "1990-2000","2000-2010","2010-2015")

dx.topic.time.1 <- cbind(rep(labels[1]),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1920,2020,by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(labels[2]),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1920,2020,by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(labels[3]),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1920,2020,by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years),rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
names(dx.topic.time) <- c("Year","Topic","Count")
dx.topic.time$Topic <- as.factor(dx.topic.time$Topic)
dx.topic.time$Count <- as.numeric(dx.topic.time$Count)

topic.decade <- ggplot(data=dx.topic.time, aes(x=Year,y=Count,group=Topic, fill=Topic))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_grey(start=0.3,end=0.7)+
  ylab("Count")+
  xlab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10), legend.key.height = unit(1, "in")) +
  guides(fill=guide_legend(title="Selected\nTopic Terms"))+
  scale_y_continuous(breaks=pretty_breaks())

## Overall topic counts

dx.topic.count <- count(dx, vars=c("topic.assign"))
dx.topic.count$freq <- as.numeric(dx.topic.count$freq)
for (i in 1:3){
    dx.topic.count$labels[dx.topic.count$topic.assign == i] <- labels[i]
}

dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq, x=labels))+
    geom_bar(stat="identity")+
      ylab("Count")+
          xlab("Topic and Associated Terms")+
              scale_y_continuous(breaks=pretty_breaks())

## Overall publication counts

### Replace "&" with "
dx$Publication.Title <- gsub("&", "and", dx$Publication.Title, ignore.case=T)

### Remove punctuation
dx$Publication.Title <- gsub("[[:punct:]]", "", dx$Publication.Title)

### Remove extra spaces
dx$Publication.Title <- gsub("[ ]+", " ", dx$Publication.Title, perl=T)

### Upcase
dx$Publication.Title <- toupper(dx$Publication.Title)

### Wrap names

dx$Publication.Title <- str_wrap(dx$Publication.Title, width=40)

### Publication counts
dx.journal <- count(dx, vars="Publication.Title")
dx.journal <- as.data.frame(dx.journal[order(-dx.journal$freq,dx.journal$Publication.Title),])
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title, levels=dx.journal$Publication.Title)

### Plot publications
dx.journal.hist <-ggplot(data=dx.journal[dx.journal$freq >= 15,], aes(y=freq,x=Publication.Title))+
  geom_bar(stat="identity")+
  ylab("Count")+
      xlab("Publications with more than 15 articles")+
          theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10),
                plot.margin = unit(c(.25,.25,.25,1.5), "in"))+
              scale_y_continuous(breaks=pretty_breaks())

ggsave(dx.topic.hist,file=paste(getwd(),"/figures/","topic-hist-",dx.tag,".pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/","publication-hist-",dx.tag,".pdf",sep=""))
ggsave(topic.decade,file=paste(getwd(),"/figures/","topic-hist-decade-",dx.tag,".pdf",sep=""))
dev.off()

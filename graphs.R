library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)
 
## Topic histogram per decade

dx.topic.time.1 <- cbind(rep(labels.terms[1]),
                         as.vector(stats.bin(x=dx$decade, 
                                             y=dx$topic.assign[dx$topic.assign %in% 1],
                                             breaks=seq(
                                                 min(dx$decade),
                                                 max(dx$decade)+10,
                                                 by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(labels.terms[2]),
                         as.vector(stats.bin(x=dx$decade, 
                                             y=dx$topic.assign[dx$topic.assign %in% 2],
                                             breaks=seq(
                                                 min(dx$decade),
                                                 max(dx$decade)+10,
                                                 by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(labels.terms[3]),
                         as.vector(stats.bin(x=dx$decade, 
                                             y=dx$topic.assign[dx$topic.assign %in% 3],
                                             breaks=seq(
                                                 min(dx$decade),
                                                 max(dx$decade)+10,
                                                 by=10))[[3]][1,]))
dx.topic.time <- as.data.frame(cbind(rep(labels.decades),
                                     rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
names(dx.topic.time) <- c("Decade","Topic","Count")
dx.topic.time$Topic <- as.factor(dx.topic.time$Topic)
dx.topic.time$Count <- as.numeric(dx.topic.time$Count)

topic.decades <- ggplot(data=dx.topic.time, aes(x=Decade, y=Count, group=Topic, fill=Topic))+
    geom_bar(stat="identity", position="dodge")+
        scale_fill_grey(start=0.3,end=0.7)+
            ylab("Count")+
                xlab("")+
                    theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10),
                          legend.key.height = unit(1, "in")) +
                        guides(fill=guide_legend(title="Selected\nTopic Terms"))+
                            scale_y_continuous(breaks=pretty_breaks())

## Overall topic counts

dx.topic.count <- count(dx, vars=c("topic.assign"))
dx.topic.count$freq <- as.numeric(dx.topic.count$freq)
for (i in 1:3){
    dx.topic.count$labels.terms[dx.topic.count$topic.assign == i] <- labels.terms[i]
}

dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq, x=labels.terms))+
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
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title,
                                       levels=dx.journal$Publication.Title)

### Plot publications
dx.journal.hist <-ggplot(data=dx.journal[dx.journal$freq >= 10,], aes(y=freq,x=Publication.Title))+
    geom_bar(stat="identity")+
        ylab("Count")+
            xlab("Publications")+
                theme(axis.text.y = element_text(size = 7))+
                    coord_flip()

ggsave(dx.topic.hist,file=paste(getwd(),"/figures/","topics-",dx.tag,".pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/","publications-",dx.tag,".pdf",sep=""))
ggsave(topic.decades,file=paste(getwd(),"/figures/","topics-decades-",dx.tag,".pdf",sep=""))
dev.off()

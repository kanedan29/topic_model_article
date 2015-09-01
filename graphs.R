library(stringr)
library(reshape2)
library(plyr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)
 
## Topic histogram per decade

dx.topic.time <- count(dx, vars=.(Decade, Topic))
dx.topic.time$Decade <- as.character(dx.topic.time$Decade)
dx.topic.time$Topic <- as.factor(dx.topic.time$Topic)

topic.decades <- ggplot(data=dx.topic.time,
                        aes(x=Decade, y=freq, group=Topic, fill=Topic))+
    geom_bar(stat="identity", position="dodge")+
        scale_fill_manual(values=grey.colors(3, start=.3, end=.7))+
            ylab("Count")+
                theme(axis.text.x=element_text(angle=45,
                          hjust=1, vjust=1, size=10),
                      legend.key.height=unit(1, "in")) +
                    guides(fill= guide_legend(title="Selected\nTopic Terms"))

dx.topic.time.all <- rbind(dx.topic.time.all,
                           data.frame(Title = dx.tag, dx.topic.time))

## Overall topic counts

dx.topic.count <- count(dx, vars=.(Topic))

dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq, x=Topic))+
    geom_bar(stat="identity")+
        ylab("Count")+
            xlab("Topic and Associated Terms")+
                scale_y_continuous(breaks=pretty_breaks())

dx.topic.count.all <- rbind(dx.topic.count.all,
                           data.frame(Title = dx.tag, dx.topic.count))

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
dx.journal <- count(dx, vars=.(Publication.Title))
dx.journal <- as.data.frame(dx.journal[order(-dx.journal$freq,dx.journal$Publication.Title),])
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title,
                                       levels=dx.journal$Publication.Title)

### Plot publications
dx.journal.hist <-ggplot(data=dx.journal[1:5,], aes(y=freq,x=Publication.Title))+
    geom_bar(stat="identity")+
        ylab("Count")+
        xlab("Publications")+
        theme(axis.text.y = element_text(size = 7))+
        coord_flip()+
        scale_y_discrete()

ggsave(dx.topic.hist,file=paste(getwd(),"/figures/","topics-",dx.tag,".pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/","publications-",dx.tag,".pdf",sep=""))
ggsave(topic.decades,file=paste(getwd(),"/figures/","topics-decades-",dx.tag,".pdf",sep=""))
dev.off()


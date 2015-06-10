library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)

  
### topic assignment histograms over time #####
years <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
                     "1990-2000","2000-2010","2010-2015")

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1920,2020,by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1920,2020,by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1920,2020,by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years),rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
names(dx.topic.time) <- c("Year","Topic","Count")
dx.topic.time$Topic <- as.factor(dx.topic.time$Topic)
dx.topic.time$Count <- as.numeric(dx.topic.time$Count)


topic.year <- ggplot(data=dx.topic.time, aes(x=Year,y=Count,group=Topic, fill=Topic))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_grey(start=0.3,end=0.7)+
  ylab("Count")+
  xlab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10)) +
  scale_y_continuous(breaks=pretty_breaks())
topic.year

##### METADATA GRAPHS #######

dx.topic.count <- count(dx, vars=c("topic.assign"))
dx.topic.count$freq <- as.numeric(dx.topic.count$freq)

dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq))+
  geom_bar(aes(x=topic.assign),fill="grey", color="black", stat="identity")+
  ylab("Count")+
  xlab("Topic")+
  scale_y_continuous(breaks=pretty_breaks())
dx.topic.hist

fill.colors <- brewer.pal(9, "Greens")[5:9]
dx.journal <- count(dx, vars="Publication.Title")
dx.journal <- as.data.frame(dx.journal[order(-dx.journal$freq,dx.journal$Publication.Title),])
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title, levels=dx.journal$Publication.Title)

dx.journal.hist <-ggplot(data=dx.journal[1:5,], aes(y=freq,fill=Publication.Title))+
  geom_bar(aes(x=Publication.Title),stat="identity")+
  ylab("Count")+
  xlab("")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_fill_manual(values=alpha(fill.colors), name="Journal")+
  scale_y_continuous(breaks=pretty_breaks())
dx.journal.hist


ggsave(dx.topic.hist,file=paste(getwd(),"/figures/",dx.tag,"_topic_hist.pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/",dx.tag,"_journal_hist.pdf",sep=""))
ggsave(topic.year,file=paste(getwd(),"/figures/",dx.tag,"_topic_year.pdf",sep=""))



#ggsave(dx.year.line,file=paste(getwd(),"/figures/",dx.tag,"_year_line.pdf",sep=""))
#ggsave(dx.year.bar,file=paste(getwd(),"/figures/",dx.tag,"_year_bar.pdf",sep=""))


#dx.year <- count(dx, vars=c("Publication.Year"))


#dx.year.line <- ggplot(data=dx.year[dx.year$Publication.Year >= 1900,], aes(x=Publication.Year, y=freq))+
 # geom_line(color="black")+
  #ylab("Count")+
  #xlab("")
#dx.year.line

#dx.year.bar <- ggplot(data=dx.year[dx.year$Publication.Year >= 1900,], aes(x=Publication.Year, y=freq))+
 # geom_bar(stat="identity", fill="grey",color="black")+
  #ylab("Count")+
  #xlab("")
#dx.year.bar


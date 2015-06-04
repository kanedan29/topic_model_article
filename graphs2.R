library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)

dx <- all[[1]]
dx.tag <- names(all)[[1]]
dx$topic.assign <- dx$Gibbs_assign

### topic assignment histograms over time #####
years <- cbind(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                         y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1900,2020,by=10))[[1]])

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1900,2020,by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1900,2020,by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1900,2020,by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years),rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
names(dx.topic.time) <- c("Year","Topic","Count")
dx.topic.time$Topic <- as.factor(dx.topic.time$Topic)

topic.year <- ggplot(data=dx.topic.time, aes(x=Year,y=Count,group=Topic, fill=Topic))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_grey(start=0.3,end=0.7)+
  ylab("Count")+
  xlab("")
topic.year

##### METADATA GRAPHS #######
dx.topic.count <- count(dx, vars=c("topic.assign"))

dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq))+
  geom_bar(aes(x=topic.assign),fill="grey", color="black", stat="identity")+
  ylab("Count")+
  xlab("Topic")
dx.topic.hist


library(doBy)
dx.year <- ldply(.data=all,.fun=count, vars=c("Publication.Year"))
dx.year2 <- split(dx.year,f=cut(dx.year$Publication.Year, breaks=seq(1900,2020,by=10)))
dx.year3 <- llply(dx.year2[3:12], summaryBy, formula=freq~.id,FUN=sum)
dx.year3 <- list()



test2 <- summaryBy(formula=freq~.id, data=test, FUN=sum)

dx.year.line <- ggplot(data=dx.year[dx.year$Publication.Year >= 1900,], aes(x=Publication.Year, y=freq))+
  geom_line(color="black")+
  ylab("Count")+
  xlab("")
dx.year.line

dx.year.bar <- ggplot(data=dx.year[dx.year$Publication.Year >= 1900,], aes(x=Publication.Year, y=freq, fill=.id))+
  geom_bar(stat="identity",position="stack")+
  ylab("Count")+
  xlab("")
dx.year.bar



dx.journal <- count(dx, vars="Publication.Title")
dx.journal <- as.data.frame(dx.journal[order(-dx.journal$freq,dx.journal$Publication.Title),])
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title, levels=dx.journal$Publication.Title)

dx.journal.hist <-ggplot(data=dx.journal[1:5,], aes(y=freq))+
  geom_bar(aes(x=Publication.Title),fill="grey", color="black",stat="identity")+
  ylab("Count")+
  xlab("")+
  theme(axis.text.x=element_text(angle=325, hjust=0, size=10),
        plot.margin=unit(x=c(1,1,0,0), units="in"))
dx.journal.hist


ggsave(dx.topic.hist,file=paste(getwd(),"/figures/",dx.tag,"_topic_hist.pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/",dx.tag,"_journal_hist.pdf",sep=""))
ggsave(dx.year.line,file=paste(getwd(),"/figures/",dx.tag,"_year_line.pdf",sep=""))
ggsave(dx.year.bar,file=paste(getwd(),"/figures/",dx.tag,"_year_bar.pdf",sep=""))
ggsave(topic.year,file=paste(getwd(),"/figures/",dx.tag,"_topic_year.pdf",sep=""))

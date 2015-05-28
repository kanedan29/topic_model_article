dx.topic.hist <-ggplot(data=dx.topic.count,aes(y=freq))+
  geom_bar(aes(x=topic_assign),fill="grey", color="black", stat="identity")+
  ylab("")+
  xlab("")
dx.topic.hist

dx$topic.assign <- topic_assign$topic_assign


### topic assignment histograms over time #####
years <- cbind(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                         y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1900,2010,by=5))[[1]])

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1900,2010,by=5))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1900,2010,by=5))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1900], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1900,2010,by=5))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years),rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
names(dx.topic.time) <- c("year","topic","count")
dx.topic.time$topic <- as.factor(dx.topic.time$topic)

topic.year <- ggplot(data=dx.topic.time, aes(x=year,y=count,group=topic, fill=topic))+
  geom_bar(stat="identity", position="dodge")
topic.year

##### METADATA GRAPHS #######
dx.year <- count(dx, vars=c("Publication.Year", "topic.assign"))

dx.year.topic.line <- ggplot(data=dx.year, aes(Publication.Year, fill=topic.assign))+
  geom_bar(position="dodge", binwidth=5)+
  ylab("Count")+
  xlab("")
dx.year.topic.line


dx.year.line <- ggplot(data=dx.year[dx.year$Publication.Year >= 1900,], aes(x=Publication.Year, y=freq))+
  geom_line(color="black")+
  ylab("Count")+
  xlab("")
dx.year.line

year.hist <- hist(dx.year$Publication.Year, breaks=seq(1900,2010,by=10))

dx.journal <- count(dx, vars="Publication.Title")
dx.journal <- as.data.frame(dx.journal[order(-dx.journal$freq,dx.journal$Publication.Title),])
dx.journal$Publication.Title <- factor(dx.journal$Publication.Title, levels=dx.journal$Publication.Title)

dx.journal.hist <-ggplot(data=dx.journal[1:5,], aes(y=freq))+
  geom_bar(aes(x=Publication.Title),fill="grey", color="black",stat="identity")+
  ylab("")+
  xlab("")+
  theme(axis.text.x=element_text(angle=325, hjust=0, size=10),
        plot.margin=unit(x=c(1,1,0,0), units="in"))
dx.journal.hist


ggsave(dx.topic.hist,file=paste(getwd(),"/figures/",dx.tag,"_topic_hist.pdf",sep=""))
ggsave(dx.journal.hist,file=paste(getwd(),"/figures/",dx.tag,"_journal_hist.pdf",sep=""))
ggsave(dx.year.line,file=paste(getwd(),"/figures/",dx.tag,"_year_line.pdf",sep=""))

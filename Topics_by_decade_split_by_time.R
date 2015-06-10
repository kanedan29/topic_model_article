library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)

years <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
           "1990-2000","2000-2010","2010-2015")

##### 1920-1960 ####

dx <- all[[1]]
dx.tag <- names(all)[[1]]
dx$topic.assign <- dx$Gibbs_assign

### topic assignment histograms over time #####

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1920,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1920,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1920,max(dx$Publication.Year),by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years)[1:4],rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
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

ggsave(topic.year,file=paste(getwd(),"/figures/",dx.tag,"_topic_year.pdf",sep=""))

#### 1960-1990 #####

dx <- all[[2]]
dx.tag <- names(all)[[2]]
dx$topic.assign <- dx$Gibbs_assign

### topic assignment histograms over time #####
years <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
           "1990-2000","2000-2010","2010-2015")

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1960,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1960,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1960,max(dx$Publication.Year),by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years)[5:7],rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
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

ggsave(topic.year,file=paste(getwd(),"/figures/",dx.tag,"_topic_year.pdf",sep=""))



#### 1990 - 2015 ####


dx <- all[[3]]
dx.tag <- names(all)[[3]]
dx$topic.assign <- dx$Gibbs_assign

### topic assignment histograms over time #####
years <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
           "1990-2000","2000-2010","2010-2015")

dx.topic.time.1 <- cbind(rep(1),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 1],breaks=seq(1990,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.2 <- cbind(rep(2),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 2],breaks=seq(1990,max(dx$Publication.Year),by=10))[[3]][1,]))
dx.topic.time.3 <- cbind(rep(3),as.vector(stats.bin(x=dx$Publication.Year[dx$Publication.Year >= 1920], 
                                                    y=dx$topic.assign[dx$topic.assign %in% 3],breaks=seq(1990,max(dx$Publication.Year),by=10))[[3]][1,]))

dx.topic.time <- as.data.frame(cbind(rep(years)[8:10],rbind(dx.topic.time.1,dx.topic.time.2,dx.topic.time.3)))
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

ggsave(topic.year,file=paste(getwd(),"/figures/",dx.tag,"_topic_year.pdf",sep=""))



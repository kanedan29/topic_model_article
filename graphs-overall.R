library(ggplot2)

## Create topic-decades-all plot

dx.topic.time.all$Title <- as.character(dx.topic.time.all$Title)
dx.topic.time.all$Title[dx.topic.time.all$Title == "grain"] <- "perennial\ngrain"
dx.topic.time.all$Title[dx.topic.time.all$Title == "pp"] <- "pigeon\npea"

topic.decades.all <- ggplot(data=dx.topic.time.all,
                             aes(x=Decade, y=freq, group=Topic, fill=Topic))+
    geom_bar(stat="identity", position="dodge")+
        ylab("Count")+
            facet_grid(Title ~ ., scales="free_y", space="free_y")+
                theme(axis.text.x=element_text(angle=45,
                          hjust=1,vjust=1,size=10),
                      legend.key.height = unit(1, "in")) +
                    guides(fill=guide_legend(title="Selected Topic Terms",
                               ncol = 3, byrow=T))

ggsave(topic.decades.all,file=paste(getwd(),"/figures/","topics-decades-all-",file.name,".pdf",sep=""))
dev.off()

## Create topic-count-all plot

dx.topic.count.all$Title <- as.character(dx.topic.count.all$Title)
dx.topic.count.all$Title[dx.topic.count.all$Title == "grain"] <- "perennial\ngrain"
dx.topic.count.all$Title[dx.topic.count.all$Title == "pp"] <- "pigeon\npea"

topic.count.all <- ggplot(data=dx.topic.count.all,
                          aes(x=Topic, y=freq, group=Title))+
    geom_bar(stat="identity")+
        ylab("Count")+
            xlab("Topic and Associated Terms")+
                facet_wrap( ~ Title, scales="free_x")+
                    theme(axis.text.x=element_text(size=10))

ggsave(topic.count.all,file=paste(getwd(),"/figures/","topics-count-all-",file.name,".pdf",sep=""))
dev.off() 

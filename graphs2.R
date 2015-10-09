library(stringr)
library(reshape2)
library(plyr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)


library(doBy)
names(all) <- c("Perennial Grains","Pigeonpea","Rice","Rye","Sorghum","Wheat")
dx.year <- ldply(.data=all,.fun=count, vars=c("Publication.Year"))
dx.year$freq <- as.numeric(dx.year$freq)
dx.year2 <- split(dx.year,f=cut(dx.year$Publication.Year, breaks=seq(1929,2019,by=10)))
test <- summaryBy(data=dx.year2[[6]], formula=freq~.id,FUN=sum)
dx.year3 <- list()
for(i in 1:length(dx.year2)){
  dx.year3[[i]] <- summaryBy(data=dx.year2[[i]],formula=freq~.id,FUN=sum)
}

names(dx.year3) <- c("1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989",
                     "1990-1999","2000-2009","2010-2015")

dx.year4 <- melt(dx.year3)
fill.colors <- brewer.pal(9, "Greys")[4:9]

dx.year.bar <- ggplot(data=dx.year4, aes(x=L1,y=value,fill=.id))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=alpha(fill.colors),
                    name="")+
  ylab("Count")+
  xlab("Decade")+
  theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10)) 

ggsave(dx.year.bar,file=paste(getwd(),"/figures/","publications-stacked-all.pdf",sep=""))
dev.off()

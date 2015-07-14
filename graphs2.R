library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(fields)
library(RColorBrewer)
library(scales)


library(doBy)
names(all) <- c("Perennial Grains","Pigeonpea","Rice","Rye","Sorghum","Wheat")
dx.year <- ldply(.data=all,.fun=count, vars=c("Publication.Year"))
dx.year$freq <- as.numeric(dx.year$freq)
dx.year2 <- split(dx.year,f=cut(dx.year$Publication.Year, breaks=seq(1900,2020,by=10)))
test <- summaryBy(data=dx.year2[[6]], formula=freq~.id,FUN=sum)
dx.year3 <- list()
for(i in 3:length(dx.year2)){
  dx.year3[[i]] <- summaryBy(data=dx.year2[[i]],formula=freq~.id,FUN=sum)
}
dx.year3 <- dx.year3[3:12]
names(dx.year3) <- c("1920-1930","1930-1940","1940-1950","1950-1960","1960-1970","1970-1980","1980-1990",
                     "1990-2000","2000-2010","2010-2015")

dx.year4 <- melt(dx.year3)
fill.colors <- brewer.pal(6, "Greens")

dx.year.bar <- ggplot(data=dx.year4, aes(x=L1,y=value,fill=.id))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=alpha(fill.colors),
                    name="")+
  ylab("")+
  xlab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1,size=10)) 

ggsave(dx.year.bar,file=paste(getwd(),"/figures/","publications-stacked-all.pdf",sep=""))
dev.off()

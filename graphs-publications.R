## Packages
## install.packages("RColorBrewer")
## install.packages("ggplot2")
require(RColorBrewer)
require(ggplot2)
require(reshape2)
require(plyr)

## Import Data
d <- data.frame(year = read.csv("P_grain_biblio_paper_database.csv")$Publication.Year,
                publication = as.character(
                    read.csv("P_grain_biblio_paper_database.csv")$Publication.Title),
                tags = read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags
                )

## Replace "&" with "and"
d$publication <- gsub("&", "and", d$publication, ignore.case=T)

## Remove punctuation
d$publication <- gsub("[[:punct:]]", "", d$publication)

## Remove extra spaces
d$publication <- gsub("[ ]+", " ", d$publication, perl=T)

## upcase
d$publication <- toupper(d$publication)

## journal titles per period

d$period[d$year <= 1960] <- "1914-1960"
d$period[d$year >= 1961 & d$year <= 1990] <- "1961-1990"
d$period[d$year >= 1991 & d$year <= 2015] <- "1991-2015"

## list per crop

d.list <- list(Grain = d[grep("search.grain", d$tags),][,c(2,4)],
               Pigeonpea = d[grep("search.pigeonpea", d$tags),][,c(2,4)],
               Rice = d[grep("search.rice", d$tags),][,c(2,4)],
               Sorghum = d[grep("search.sorghum", d$tags),][,c(2,4)],
               Wheat = d[grep("search.wheat", d$tags),][,c(2,4)],
               All = d[,c(2,4)])

## Summarize data: (1) count publications per period, (2) calculate
## the total publication count for the period, (3) normalize counts

d.list.summary <- lapply(d.list, ddply, .(period, publication),
                         summarise,
                         n.pub = length(publication))

d.list.summary <- lapply(d.list.summary, ddply, .(period),
                         mutate,
                         n.period = length(publication),
                         n.norm = n.pub/n.period)

lapply(d.list.summary, transform, order(-d.list$i$n.norm,d.list$i$publication),])
     }


for (i in names(d.list.summary)){
transform(d.list.summary$i, d.list.summary$All[with(d.list.summary$All, order(period,-n.norm, publication)),])
}


publication = strwrap(publication, width=.3)

## Create Plots

for (i in names(d.list){
         g <- ggplot(data=d.list$i, aes(y=freq,fill=Publication.Title))
g + geom_bar(aes(x=Publication.Title),stat="identity")+
  ylab("Count")+
  xlab("")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_fill_manual(values=alpha(fill.colors), name="Journal")+
  scale_y_continuous(breaks=pretty_breaks())
dx.journal.hist



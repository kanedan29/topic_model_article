## Import Data
d <- data.frame(year = read.csv("P_grain_biblio_paper_database.csv")$Publication.Year,
                Publication = as.character(
                    read.csv("P_grain_biblio_paper_database.csv")$Publication.Title),
                tags = read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags
                )

## Replace "&" with "and"
d$Publication <- gsub("&", "and", d$Publication, ignore.case=T)

## Remove punctuation
d$Publication <- gsub("[[:punct:]]", "", d$Publication)

## Remove extra spaces
d$Publication <- gsub("[ ]+", " ", d$Publication, perl=T)

## upcase
d$Publication <- toupper(d$Publication)

## journal titles per period

d$period[d$year <= 1960] <- "1914-1960"
d$period[d$year >= 1961 & d$year <= 1990] <- "1961-1990"
d$period[d$year >= 1991 & d$year <= 2015] <- "1991-2015"

## list per crop

d.list <- list(grain = d[grep("search.grain", d$tags),][,c(2,4)],
               pigeonpea = d[grep("search.pigeonpea", d$tags),][,c(2,4)],
               rice = d[grep("search.rice", d$tags),][,c(2,4)],
               rye = d[grep("search.rye", d$tags),][,c(2,4)],
               sorghum = d[grep("search.sorghum", d$tags),][,c(2,4)],
               wheat = d[grep("search.wheat", d$tags),][,c(2,4)],
               all = d[,c(2,4)])

## Count publications per period,

require(reshape2)
require(plyr)

d.list.summary <- lapply(d.list, ddply, .(period, Publication),
                         summarise,
                         Count = length(Publication))

## Calculate publication count for the period, and normalize counts

d.list.summary <- lapply(d.list.summary, ddply, .(period),
                         mutate,
                         n.period = length(Publication),
                         n.norm = Count/n.period)

## Flatten, arrange dataframe

d.df.summary <- ldply(d.list.summary)
d.df.summary <- arrange(d.df.summary, .id, desc(period), desc(n.norm),
                        Publication)

## Select top 10 publications per time period

d.df.summary <- ddply(d.df.summary, .(.id, period), function(x) x[1:10,])

detach(package:reshape2, unload=T)
detach(package:plyr, unload=T)

## Wrap names

require(stringr)
d.df.summary$Publication <- str_wrap(d.df.summary$Publication, width=40)
detach(package:stringr, unload=T)

## Select complete cases

d.df.summary <- d.df.summary[complete.cases(d.df.summary),]

## Create Plots

require(ggplot2)
require(grid)

for (i in unique(d.df.summary$.id)){

    g <- ggplot(d.df.summary[d.df.summary$.id == i,],
                aes(x=Publication, y=Count))+
        geom_bar(stat="identity")+
            facet_grid(. ~ period, scale = "free_y")+
                theme(axis.text.y = element_text(size = 7))+
                coord_flip()

    ggsave(g,file=paste(getwd(),"/figures/","publications-",
                 i,".pdf",sep=""),
           width = 7, height = 9, units = "in")

}

detach(package:ggplot2, unload=T)
detach(package:grid, unload=T)

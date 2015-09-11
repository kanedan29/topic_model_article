## Import Data
d <- data.frame(year = read.csv("P_grains_relevant.csv")$Publication.Year,
                Publication = read.csv("P_grains_relevant.csv")$Publication.Title,
                tags = read.csv("P_grains_relevant.csv")$Manual.Tags)

d$Publication <- as.character(d$Publication)

d_replace <- read.csv("pairs/pairs-j-em-replace.csv", colClasses = "character")

## Replace from replace with replacement

for (i in 1:nrow(d_replace)){
    d$Publication[d$Publication == d_replace$replace[i]] <- d_replace$replacement[i]
}

## Replace "&" with "and"
d$Publication <- gsub("&", "and", d$Publication, ignore.case=T)

## Remove punctuation
d$Publication <- gsub("[[:punct:]]", "", d$Publication)

## Remove extra spaces
d$Publication <- gsub("[ ]+", " ", d$Publication, perl=T)

## upcase
d$Publication <- toupper(d$Publication)

## journal titles per period

require(reshape2)
require(plyr)

dates.min <- c(min(d$year), min(d$year)+30*c(1:2)+1)
dates.max <- c(min(d$year)+30*c(1:2),max(d$year))
dates <- c(1929+30*c(0:2), max(d$year))

d.list <- split(d,f=cut(d$year, breaks=dates, labels = paste(dates.min, dates.max, sep= "-")))
d <- ldply(d.list, .id="period")

## list per crop

d.list <- list(grain = d[grep("search.grain", d$tags),][,c("Publication","period")],
               pigeonpea = d[grep("search.pigeonpea", d$tags),][,c("Publication","period")],
               rice = d[grep("search.rice", d$tags),][,c("Publication","period")],
               rye_wheat = d[sort(
                   unique(unlist(sapply(c("search.rye", "search.wheat"),
                                        grep, d$tags), recursive=T))),][,c("Publication","period")],
               sorghum = d[grep("search.sorghum", d$tags),][,c("Publication","period")],
               all = d[,c("Publication","period")])

## Count publications per period

d.list.summary <- lapply(d.list, ddply, .(period, Publication),
                         summarise,
                         Count = length(Publication))

## Calculate publication count for the period, and normalize counts

d.list.summary <- lapply(d.list.summary, ddply, .(period),
                         mutate,
                         n.period = length(Publication),
                         n.norm = Count/n.period)

## Flatten, arrange dataframe

d.df.summary <- ldply(d.list.summary, .id="key")
d.df.summary <- arrange(d.df.summary, key, desc(period), desc(n.norm),
                        Publication)

## Select top 5 publications per time period

d.df.summary <- ddply(d.df.summary, .(key, period), function(x) x[1:5,])

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

for (i in unique(d.df.summary$key)){

    g <- ggplot(d.df.summary[d.df.summary$key == i,],
                aes(x=Publication, y=Count))+
        geom_bar(stat="identity")+
            facet_grid(. ~ period, scale = "free_y")+
                theme(axis.text.y = element_text(size = 10))+
                coord_flip()

    ggsave(g,file=paste(getwd(),"/figures/","publications-3periods-",
                 i,".pdf",sep=""), width = 7, height = 7, units = "in")

}

detach(package:ggplot2, unload=T)

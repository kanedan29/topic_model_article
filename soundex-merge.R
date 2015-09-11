### load libraries and load tags
library(RecordLinkage)
source("tags2.R")

### read in data

d <- read.csv("P_grains_relevant.csv", na.strings="")
d <- d[!is.na(d$Abstract.Note),]

d.short <- d[,c("Key", "Publication.Year", "Author", "Title",
                "Publication.Title", "Abstract.Note", "Date", "Issue",
                "Volume")]

### find pairs of records (r)

pairs.r <- compare.dedup(d.short,
                         blockfld=list(c(2:4),c(2,3,5),c(2,3,6),c(2,4,5),c(2,4,6)),
                         phonetic=c(3:6),
                         phonfun=soundex,
                         exclude=c(1,7:9))

summary(pairs.r) # 75 pairs

## use the EM to calculate weights.Cutoff for soundex is not
## recommended, so setting it high to 0.99.

pairs.r.em <- emWeights(pairs.r, cutoff=0.99)

hist(pairs.r.em$Wdata, plot=FALSE) # high counts at brks -4,0,6,10,16

## Classify (choose to include all)

pairs.r.em <- emClassify(pairs.r.em,
                         threshold.lower=-4)
## Summarize.

summary(pairs.r.em) # 75 possible

## Write file

write.csv(getPairs(pairs.r.em, show="possible"),
          "./pairs/pairs-r-em.csv")

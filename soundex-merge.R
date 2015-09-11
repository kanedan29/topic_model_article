### load libraries and load tags
library(RecordLinkage)
source("tags2.R")

### read in data

dj <- unique(read.csv("P_grains_relevant.csv", na.strings="")["Publication.Title"])

d <- read.csv("P_grains_relevant.csv", na.strings="")
d <- d[!is.na(d$Abstract.Note),]

d.short <- d[,c("Key", "Publication.Year", "Author", "Title",
                "Publication.Title", "Abstract.Note", "Date", "Issue",
                "Volume")]

### find pairs of journals (j) records (r)

pairs.j <- compare.dedup(dj,
                         blockfld=1,
                         phonetic=1,
                         phonfun=soundex)

pairs.r <- compare.dedup(d.short,
                         blockfld=list(c(2:4),c(2,3,5),c(2,3,6),c(2,4,5),c(2,4,6)),
                         phonetic=c(3:6),
                         phonfun=soundex,
                         exclude=c(1,7:9))

summary(pairs.j) # 2021 pairs
summary(pairs.r) # 75 pairs

## use the EM to calculate weights.Cutoff for soundex is not
## recommended, so setting it high to 0.99.

pairs.j.em <- emWeights(pairs.j, cutoff=0.99)

pairs.r.em <- emWeights(pairs.r, cutoff=0.99)

hist(pairs.j.em$Wdata, plot=FALSE) # high counts at brks -6, 8
hist(pairs.r.em$Wdata, plot=FALSE) # high counts at brks -4,0,6,10,16

## Classify (choose to include all)

pairs.j.em <- emClassify(pairs.j.em,
                         threshold.lower=-6)

pairs.r.em <- emClassify(pairs.r.em,
                         threshold.lower=-4)
## Summarize.

summary(pairs.j.em) # 2021 possible
summary(pairs.r.em) # 75 possible

## Write file

write.csv(matrix(getPairs(
    pairs.j.em, show="possible")$Publication.Title,
                 ncol=3, byrow=T)[,1:2],
          "./pairs/pairs-j-em.csv")

write.csv(getPairs(pairs.r.em, show="possible"),
          "./pairs/pairs-r-em.csv")

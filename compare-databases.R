## current master: 73a23c87c175f1f1e1acb3027e24187491f24a4e original
## DB: 148ac56ef012af4f507b840392746e061bf21dba retrieved database and
## changed name to *-148ac56e.csv, which is also now tracked as a
## separate file by github.

## Load libraries and source objects
library(stringr)
source("tags2.R")

## Import databases
db.old <- read.csv("P_grain_biblio_paper_database-148ac56e.csv")
db.new <- read.csv("P_grain_biblio_paper_database.csv")

## Make table of articles removed. Note that this is just based on the
## Key field. For the most part, this is correct except where entries
## were merged in Zotero, thus creating a new Key. The db.old includes
## 3026 entries and db.new includes 2696 entries for a difference of
## 330 entries. However, 452 entries are reported to differ in terms
## of Keys, with a difference of 122 entries due to merging.

db.diff <- db.old[!db.old$Key %in% db.new$Key,
                  c("Key", "Author", "Title", "Publication.Year",
                    "Abstract.Note", "Manual.Tags")]

## Compare length: 452 removed and/or merged as of 2015-07-27
dim(db.diff)[1]

## Make tags object and sort tags by frequency
db.diff$Manual.Tags <- gsub(", ", "; ", db.diff$Manual.Tags)
tags <- data.frame(table(tolower(unlist(str_split(db.diff$Manual.Tags,"; ")))))
colnames(tags) <- c("Tags", "Frequency")
tags <- tags[with(tags, order(-Frequency)),]
tags <- tags[tags$Tags != "spv",]

## Examine counts from searches
write.csv(tags[tags$Tags %in% all.tags,],
          file="./db-comparison/searches.csv", row.names=F)

## Examine counts of most frequent tags. Easier to manually remove
## those rows associated with searches, rather than code it.

write.csv(tags[tags$Frequency >= 5,],
          file="./db-comparison/keywords.csv", row.names=F)

## Subset by Database and Crop

list.db <- c("AGRICOLA","ScienceDirect","WOS")
list.tags <- c("rye|secale","pigeon|cajanus","wheat|triticum","rice|oryza")

new.wos.rye <- db.new[grep("(?=.*WOS)(?=.*rye|secale)",db.new$Manual.Tags,perl=T),]
new.wos.ppe <- db.new[grep("(?=.*WOS)(?=.*pigeon|cajanus)",db.new$Manual.Tags,perl=T),]
new.wos.rce <- db.new[grep("(?=.*WOS)(?=.*rice|oryza)",db.new$Manual.Tags,perl=T),]
new.wos.wht <- db.new[grep("(?=.*WOS)(?=.*wheat|triticum)",db.new$Manual.Tags,perl=T),]

new.scd.rye <- db.new[grep("(?=.*ScienceDirect)(?=.*rye|secale)",db.new$Manual.Tags,perl=T),]
new.scd.ppe <- db.new[grep("(?=.*ScienceDirect)(?=.*pigeon|cajanus)",db.new$Manual.Tags,perl=T),]
new.scd.rce <- db.new[grep("(?=.*ScienceDirect)(?=.*rice|oryza)",db.new$Manual.Tags,perl=T),]
new.scd.wht <- db.new[grep("(?=.*ScienceDirect)(?=.*wheat|triticum)",db.new$Manual.Tags,perl=T),]

new.agr.rye <- db.new[grep("(?=.*AGRICOLA)(?=.*rye|secale)",db.new$Manual.Tags,perl=T),]
new.agr.ppe <- db.new[grep("(?=.*AGRICOLA)(?=.*pigeon|cajanus)",db.new$Manual.Tags,perl=T),]
new.agr.rce <- db.new[grep("(?=.*AGRICOLA)(?=.*rice|oryza)",db.new$Manual.Tags,perl=T),]
new.agr.wht <- db.new[grep("(?=.*AGRICOLA)(?=.*wheat|triticum)",db.new$Manual.Tags,perl=T),]

old.wos.rye <- db.old[grep("(?=.*WOS)(?=.*rye|secale)",db.old$Manual.Tags,perl=T),]
old.wos.ppe <- db.old[grep("(?=.*WOS)(?=.*pigeon|cajanus)",db.old$Manual.Tags,perl=T),]
old.wos.rce <- db.old[grep("(?=.*WOS)(?=.*rice|oryza)",db.old$Manual.Tags,perl=T),]
old.wos.wht <- db.old[grep("(?=.*WOS)(?=.*wheat|triticum)",db.old$Manual.Tags,perl=T),]

old.scd.rye <- db.old[grep("(?=.*ScienceDirect)(?=.*rye|secale)",db.old$Manual.Tags,perl=T),]
old.scd.ppe <- db.old[grep("(?=.*ScienceDirect)(?=.*pigeon|cajanus)",db.old$Manual.Tags,perl=T),]
old.scd.rce <- db.old[grep("(?=.*ScienceDirect)(?=.*rice|oryza)",db.old$Manual.Tags,perl=T),]
old.scd.wht <- db.old[grep("(?=.*ScienceDirect)(?=.*wheat|triticum)",db.old$Manual.Tags,perl=T),]

old.agr.rye <- db.old[grep("(?=.*AGRICOLA)(?=.*rye|secale)",db.old$Manual.Tags,perl=T),]
old.agr.ppe <- db.old[grep("(?=.*AGRICOLA)(?=.*pigeon|cajanus)",db.old$Manual.Tags,perl=T),]
old.agr.rce <- db.old[grep("(?=.*AGRICOLA)(?=.*rice|oryza)",db.old$Manual.Tags,perl=T),]
old.agr.wht <- db.old[grep("(?=.*AGRICOLA)(?=.*wheat|triticum)",db.old$Manual.Tags,perl=T),]

## Tabulate length differences between databases and crops
diff.new.old <- data.frame(database = c(
                               rep("WOS",4),
                               rep("ScienceDirect",4),
                               rep("AGRICOLA",4)),
                           crop = rep(c("rye","pigeonpea","rice","wheat"),3),
                           diff.new.old = c(
                               dim(new.wos.rye)[1]-dim(old.wos.rye)[1],
                               dim(new.wos.ppe)[1]-dim(old.wos.ppe)[1],
                               dim(new.wos.rce)[1]-dim(old.wos.rce)[1],
                               dim(new.wos.wht)[1]-dim(old.wos.wht)[1],
                               dim(new.scd.rye)[1]-dim(old.scd.rye)[1],
                               dim(new.scd.ppe)[1]-dim(old.scd.ppe)[1],
                               dim(new.scd.rce)[1]-dim(old.scd.rce)[1],
                               dim(new.scd.wht)[1]-dim(old.scd.wht)[1],
                               dim(new.agr.rye)[1]-dim(old.agr.rye)[1],
                               dim(new.agr.ppe)[1]-dim(old.agr.ppe)[1],
                               dim(new.agr.rce)[1]-dim(old.agr.rce)[1],
                               dim(new.agr.wht)[1]-dim(old.agr.wht)[1])
                           )


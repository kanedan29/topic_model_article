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

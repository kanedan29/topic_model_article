## Packages
# install.packages("venneuler")
require("venneuler")

## Import Data

d <- as.character(read.csv("Zotero_db_dups_removed.csv")$Manual.Tags)

## Remove empty tags, and cassava articles

d <- d[grep("AGRICOLA|ScienceDirect|WOS", d)]
d <- d[grep("cassava|manihot", d, invert=T)]

## Area counts

areas <- c()

for (i in c("AGRICOLA","ScienceDirect","WOS")){
    areas <- c(areas,
               length(
                   d[grep(i, d)]
                   )
               )
}

## n12 counts

n12 <- d[grep("(?=.*AGRICOLA)(?=.*ScienceDirect)", d, perl=T)]
n12 <- length(n12)

## n13 counts

n13 <- d[grep("(?=.*AGRICOLA)(?=.*WOS)", d, perl=T)]
n13 <- length(n13)

## n23 counts

n23 <- d[grep("(?=.*ScienceDirect)(?=.*WOS)", d, perl=T)]
n23 <- length(n23)

## n123 counts

n123 <- d[grep("(?=.*ScienceDirect)(?=.*AGRICOLA)(?=.*WOS)", d, perl=T)]
n123 <- length(n123)

## Create the Graph
v <- venneuler(c(A=areas[1], B=areas[2], C=areas[3],
                 `A&B`=n12,`A&C`=n13,`B&C`=n23,`A&B&C`=n123)
               )              
v$labels <- c("AGRICOLA","ScienceDirect","Web of Science")

## Create the Plot

pdf("venn-databases.pdf")
plot(v)
dev.off()

## Packages
# install.packages("venneuler")
require("venneuler")

## Import Data

d <- as.character(read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags)

## Area counts

areas <- c()

for (i in c("AGRICOLA","ScienceDirect","WOS","Scopus")){
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

## n14 counts

n14 <- d[grep("(?=.*AGRICOLA)(?=.*Scopus)", d, perl=T)]
n14 <- length(n14)

## n23 counts

n23 <- d[grep("(?=.*ScienceDirect)(?=.*WOS)", d, perl=T)]
n23 <- length(n23)

## n24 counts

n24 <- d[grep("(?=.*ScienceDirect)(?=.*Scopus)", d, perl=T)]
n24 <- length(n24)

## n34 counts

n34 <- d[grep("(?=.*WOS)(?=.*Scopus)", d, perl=T)]
n34 <- length(n34)

## n123 counts

n123 <- d[grep("(?=.*AGRICOLA)(?=.*ScienceDirect)(?=.*WOS)", d, perl=T)]
n123 <- length(n123)

## n124 counts

n124 <- d[grep("(?=.*AGRICOLA)(?=.*ScienceDirect)(?=.*Scopus)", d, perl=T)]
n124 <- length(n124)

## n134 counts

n134 <- d[grep("(?=.*AGRICOLA)(?=.*WOS)(?=.*Scopus)", d, perl=T)]
n134 <- length(n134)

## n234 counts

n234 <- d[grep("(?=.*ScienceDirect)(?=.*WOS)(?=.*Scopus)", d, perl=T)]
n234 <- length(n234)

## n1234 counts

n1234 <- d[grep("(?=.*ScienceDirect)(?=.*AGRICOLA)(?=.*WOS)(?=.*Scopus)", d, perl=T)]
n1234 <- length(n1234)

## Create the Graph
v <- venneuler(c(A=areas[1], B=areas[2], C=areas[3], D=areas[4],
                 `A&B`=n12,`A&C`=n13,`A&D`=n14,`B&C`=n23,`B&D`=n24,`C&D`=n34,`A&B&C`=n123,`A&B&D`=n124,`A&C&D`=n134,`B&C&D`=n234,`A&B&C&D`=n1234)
               )
v$labels <- c("AGRICOLA","ScienceDirect","Web of Science","Scopus")

## Create the Plot

pdf("venn-databases.pdf")
plot(v)
dev.off()

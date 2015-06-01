## Packages
# install.packages("VennDiagram")
require("VennDiagram")

## Import Data

d <- as.character(read.csv("Zotero_db_dups_removed.csv")$Manual.Tags)

## Remove empty tags

d <- d[grep("AGRICOLA|ScienceDirect|WOS", d)]

## Select area counts

areas <- c()

for (i in c("AGRICOLA","ScienceDirect","WOS")){
    areas <- c(areas,
               length(
                   d[grep(i, d)]
                   )
               )
}

## Select n12 counts

n12 <- d[grep("(?=.*AGRICOLA)(?=.*ScienceDirect)", d, perl=T)]
n12 <- length(n12)

## Select n13 counts

n13 <- d[grep("(?=.*AGRICOLA)(?=.*WOS)", d, perl=T)]
n13 <- length(n13)

## Select n23 counts

n23 <- d[grep("(?=.*ScienceDirect)(?=.*WOS)", d, perl=T)]
n23 <- length(n23)

## Select n123

n123 <- d[grep("(?=.*ScienceDirect)(?=.*AGRICOLA)(?=.*WOS)", d, perl=T)]
n123 <- length(n123)

## Plot

pdf("databases.pdf")
draw.triple.venn(area1= areas[1],
                 area2= areas[2],
                 area3= areas[3],
                 n12= n12,
                 n13= n13,
                 n23= n23,
                 n123= n123
                 )
dev.off()

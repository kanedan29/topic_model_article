## Packages
## install.packages("VennDiagram")

require("VennDiagram")

## Import Data

d <- as.character(read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags)

## Create Category Names

cat.names <- c()
for (i in c("AGRICOLA","ScienceDirect","Scopus","WOS")){
    cat.names <- cbind(cat.names,
                           paste(i,"\n","(",length(d[grep(i, d)]),")",sep=""))
}

## Create the Graphs

v.quint <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("ScienceDirect",d),
         C= grep("Scopus",d),
         D= grep("WOS",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    cat.fontfamily = "sans",
    category.names = cat.names,
    force.unique = TRUE,
    )

## Create the Plot

pdf("venn-databases-four-way.pdf")
grid.draw(v.quint)
dev.off()

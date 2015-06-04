## Packages
## install.packages("VennDiagram")
## install.packages("RColorBrewer")
require("VennDiagram")
require("RColorBrewer")

## Import Data

d <- as.character(read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags)

## Create Category Names

cat.names <- c()
for (i in c("AGRICOLA","ScienceDirect","Scopus","Web of Science")){
    cat.names <- cbind(cat.names,
                           paste(i,"\n","(",length(d[grep(i, d)]),")",sep=""))
}

## Create Color Palette

fill.colors <- brewer.pal(4, "Accent")


## Create the Graphs

v.quint <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("ScienceDirect",d),
         C= grep("Scopus",d),
         D= grep("Web of Science",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    cat.fontfamily = "sans",
    category.names = cat.names,
    fill = fill.colors,
    alpha = .4,
    col = "darkgray"
    )

## Create the Plot

pdf("venn-databases.pdf")
grid.draw(v.quint)
dev.off()

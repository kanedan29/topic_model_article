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
                       paste(i,
                             "\n", "(", length(d[grep(i, d)]), ")", sep=""))
}

for (i in c("search.pigeonpea", "search.rice", "search.rye",
            "search.sorghum", "search.wheat")){
    cat.names <- cbind(cat.names,
                       paste(unlist(strsplit(i,"[.]"))[2],
                             "\n", "(", length(d[grep(i, d)]),")",sep=""))
}

## Create Color Palette

fill.colors <- brewer.pal(9, "Set3")


## Create the Graphs

v.db <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("ScienceDirect",d),
         C= grep("Scopus",d),
         D= grep("Web of Science",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    cat.fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.dist = c(.25, .25, .15, .15),
    sub.pos = c(0.09, 1.05),
    category.names = cat.names[1:4],
    sub = "(a) DATABASES",
    fill = fill.colors[1:4],
    alpha = .4,
    col = "darkgray"
    )

v.cp <- venn.diagram(
    list(A= grep("search.pigeonpea",d),
         B= grep("search.rice",d),
         C= grep("search.rye",d),
         D= grep("search.sorghum",d),
         E= grep("search.wheat",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    cat.fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.dist = .25,
    sub.pos = c(0, 1.05),
    sub = "(b) CROPS",
    category.names = cat.names[5:9],
    fill = fill.colors[5:9],
    alpha = .4,
    col = "darkgray"
    )

## Create the Plot

pdf("./figures/databases.pdf", width=4.5,height=9)
pushViewport(viewport(layout = grid.layout(2, 1), height=.85, width=.85))
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
grid.draw(v.db)
popViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
grid.draw(v.cp)
popViewport()
dev.off()

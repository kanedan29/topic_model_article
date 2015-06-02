## Packages
## install.packages("VennDiagram")

require("VennDiagram")

## Import Data

d <- as.character(read.csv("P_grain_biblio_paper_database.csv")$Manual.Tags)

## Create the Graphs

v.1 <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("ScienceDirect",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(a)",
    category.names = c("AGRICOLA","ScienceDirect"),
    sub.pos = c(0, .9),
    cat.pos = 0,
    cat.dist = 0.03,
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

v.2 <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("Scopus",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(b)",
    category.names = c("AGRICOLA","Scopus"),
    sub.pos = c(0, .9),
    cat.pos = c(-30,0),
    cat.dist = c(.05,.03),
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

v.3 <- venn.diagram(
    list(A= grep("AGRICOLA",d),
         B= grep("WOS",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(c)",
    category.names = c("AGRICOLA","Web of Science"),
    sub.pos = c(0, .9),
    cat.pos = c(60,0),
    cat.dist = c(.07,.03),
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

v.4 <- venn.diagram(
    list(A= grep("ScienceDirect",d),
         B= grep("Scopus",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(d)",
    category.names = c("ScienceDirect","Scopus"),
    sub.pos = c(0, .9),
    cat.pos = 0,
    cat.dist = .03,
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

v.5 <- venn.diagram(
    list(A= grep("ScienceDirect",d),
         B= grep("WOS",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(e)",
    category.names = c("ScienceDirect","Web of Science"),
    sub.pos = c(0, .9),
    cat.pos = c(-20,0),
    cat.dist = 0.03,
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

v.6 <- venn.diagram(
    list(A= grep("Scopus",d),
         B= grep("WOS",d)
         ),
    filename=NULL,
    fontfamily = "sans",
    sub.fontfamily = "sans",
    cat.fontfamily = "sans",
    sub = "(f)",
    category.names = c("Scopus","Web of Science"),
    sub.pos = c(0, .9),
    cat.pos = c(-30,0),
    cat.dist = c(.04,.03),
    force.unique = TRUE,
    euler.d = T,
    scaled = T
    )

## Create the Plot

pdf("venn-databases-two-way.pdf", width=6,height=9)
pushViewport(viewport(layout = grid.layout(3, 2), height=.92, width=.92))
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
grid.draw(v.1)
popViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
grid.draw(v.2)
popViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 3))
grid.draw(v.3)
popViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))
grid.draw(v.4)
popViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2))
grid.draw(v.5)
popViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 3))
grid.draw(v.6)
popViewport()
dev.off()

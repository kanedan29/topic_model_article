load("crops_combined_split_time_workspace.RData")
library(data.table)

post <- posterior(all_topics[[1]][[3]])
tops <- post[[2]]


top2 <- melt(tops)
top2<-as.data.table(top2)

top2 <- top2[top2[, .I[value == max(value)], by=Var1]$V1]
names(top2)<-c("Document number","Topic","Probability")

median(top2$Probability)

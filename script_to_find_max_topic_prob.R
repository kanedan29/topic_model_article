> post <- posterior(all_topics[[1]][[3]])
> tops <- post[[2]]


> top2 <- melt(tops)


as.data.table(top2)

top2 <- top2[top2[, .I[value == max(value)], by=Var1]$V1]
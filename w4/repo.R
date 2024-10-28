
# 設問2
example <- read.table("week4-example.txt", header=TRUE)
arrowheadline <- arrow_head_line(angle=0, lineend="butt")
cols <- colnames(example)

# 設問2, 3
histogram <- function(i) {
	c <- cols[i]
	w <- example[,c]
	m <- mean(w)
	sd <- sd(w)

	cat(c, "の平均値=", m, "\n", sep="")
	cat(c, "の標準偏差=", sd, "\n", sep="")

	ggplot(example[i], aes(x= w)) +
		geom_histogram()+
		labs(x= paste(c, "の体重", sep=""), y= "個体数") +
				geom_segment(aes(x= m,    y= -0.3, xend= m, yend= 5), colour="red")+
		geom_arrow_segment(aes(x= m-sd, y= -0.3, yend= 0), color= 2)+
		geom_arrow_segment(aes(x= m+sd, y= -0.3, yend= 0), color= 2)+
		geom_arrow_segment(aes(xend= m - sd, y= 5, x= m), color= 2)+
		geom_arrow_segment(aes(xend= m + sd, y= 5, x= m), color= 2)+
		annotate("text", x= m, y= 5.2, label="平均")+
		annotate("text", x= m-sd, y= 5.5, label="平均-標準偏差")+
		annotate("text", x= m+sd, y= 5.5, label="平均+標準偏差")+
		annotate("text", x= m, y= -0.5, label= round(m, 2))+
		annotate("text", x= m-sd, y= -0.5, label= round(m-sd, 2))+
		annotate("text", x= m+sd, y= -0.5, label= round(m+sd, 2))
}
ggs <- lapply(seq_along(example), function(i) {
	histogram(i)
})
wrap_plots(ggs)

# 設問4
print(paste("全data数", ncol(example)*nrow(example)) )

kusuris <- unlist(lapply(cols, function(c) rep(c, length(example[,c]))))
kusuri <- factor(kusuris)
list <- unlist(example, use.names=F)

res_aov <- aov(list~kusuri)
print(summary(res_aov))

# 発展課題
res_one <- oneway.test(list~kusuri, var.equal=TRUE)
print(res_one)

# 設問5
resbar <- bartlett.test(formula=list~kusuri)
print(resbar)

# 設問(6)
print(paste("対応のないときの効果量=", 283.4/2303.9))

example <- read.table("week4-example.txt", header=TRUE)
cols <- colnames(example)

# 設問2 発展課題
frame <- do.call(rbind.data.frame,
	lapply(cols, function(col) {
		data.frame(weight = example[,col], group = factor(rep(col, length(example))))
	})
)

for (col in cols) {
	w <- frame$weight[frame$group == col]
	m <- mean(w)
	sd <- sd(w)
	cat(col, "群の平均    :", round(m, 3), "\n")
	cat(col, "群の標準偏差:", round(sd, 3), "\n")
}

# 設問3
arrowheadline <- arrow_head_line(angle=0, lineend="butt")

histogram <- function(i) {
	c <- cols[i]
	w <- example[,c]
	m <- mean(w)
	sd <- sd(w)

	ggplot(example[i], aes(x= w)) +
		geom_histogram()+
		labs(x= paste(c, "の体重", sep=""), y= "個体数") +
				geom_segment(aes(x= m,    y= -0.3, xend= m, yend= 5), colour="red")+
		geom_arrow_segment(aes(x= m-sd, y= -0.3, yend= 0), color= 2)+
		geom_arrow_segment(aes(x= m+sd, y= -0.3, yend= 0), color= 2)+
		geom_arrow_segment(aes(xend= m - sd, y= 5, x= m), color= 2)+
		geom_arrow_segment(aes(xend= m + sd, y= 5, x= m), color= 2)+
		annotate("text", x= m, y= 5.2, label="平均")+
		annotate("text", x= min(w) + 3, y= 4.5, label=paste("標準偏差", round(sd, 2)))+
		annotate("text", x= m-sd, y= 5.5, label="平均-標準偏差")+
		annotate("text", x= m+sd, y= 5.5, label="平均+標準偏差")+
		annotate("text", x= m, y= -0.5, label= round(m, 2))+
		annotate("text", x= m-sd, y= -0.5, label= round(m-sd, 2))+
		annotate("text", x= m+sd, y= -0.5, label= round(m+sd, 2))
}
ggplots <- lapply(seq_along(example), function(i) {
	histogram(i)
})
wrap_plots(ggplots)

# 設問4
cat("全data数", ncol(example)*nrow(example), "\n")

res_aov <- aov(weight~group, data = frame)
print(summary(res_aov))
cat("効果量:", 191.7/2067.8, "\n")

# 発展課題
res_oneway <- oneway.test(weight~group, var.equal=TRUE, data = frame)
print(res_oneway)
res_bartlett <- bartlett.test(formula=weight~group, data = frame)
print(res_bartlett)


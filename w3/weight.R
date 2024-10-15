
weights <- read.table("weight.txt", header=FALSE, skip=1)
list <- c("A群", "B群")

arrowheadline <- arrow_head_line(angle=0, lineend="butt")

histogram <- function(i) {
	w <- weights[i][,1]
	m <- mean(w)
	sd <- sd(w)

	# 設問(2)
	cat(list[i], "の平均値=", m, "\n", sep="")
	cat(list[i], "の標準偏差=", sd, "\n", sep="")

	ggplot(weights[i], aes(x = w)) +
		geom_histogram()+
		labs(x = paste(list[i], "の体重", sep=""), y = "人数") +
				geom_segment(aes(x = m,    y = -0.3, xend = m, yend = 5), colour="red")+
		geom_arrow_segment(aes(x = m-sd, y = -0.3, yend = 0), color=2)+
		geom_arrow_segment(aes(x = m+sd, y = -0.3, yend = 0), color=2)+
		geom_arrow_segment(aes(xend = m - sd, y = 5, x = m), color = 2)+
		geom_arrow_segment(aes(xend = m + sd, y = 5, x = m), color = 2)+
		annotate("text", x= m, y= 5.2, label="平均")+
		annotate("text", x= m-sd, y= 5.5, label="平均-標準偏差")+
		annotate("text", x= m+sd, y= 5.5, label="平均+標準偏差")+
		annotate("text", x= m, y= -0.5, label= round(m, 2))+
		annotate("text", x= m-sd, y= -0.5, label= round(m-sd, 2))+
		annotate("text", x= m+sd, y= -0.5, label= round(m+sd, 2))
}
chartList <- lapply(1:2, function(i) {
	histogram(i)
})

wrap_plots(chartList)
# ggsave("chartList.png")
dataA <- weights[1][,1]
dataB <- weights[2][,1]
result <- t.test(
	x = dataA,
	y = dataB,
	paired = TRUE, # 対応がある
	var.equal = TRUE
)
print(result)
p <- result$p.value
ha <- p < 0.05
cat("対立仮説 =", ha, "\n")

result <- t.test(
	x = dataA,
	y = dataB,
	paired = FALSE, # 対応がない
	var.equal = TRUE
)
print(result)

# 分散の等質性の検定
result <- var.test(dataA, dataB)
print(result)
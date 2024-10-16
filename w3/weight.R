# 設問2
weights <- read.table("weight.txt", header=FALSE, skip=1)
list <- c("A群", "B群")
arrowheadline <- arrow_head_line(angle=0, lineend="butt")

histogram <- function(i) {
	w <- weights[i][,1]
	m <- mean(w)
	sd <- sd(w)
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

chart_list <- lapply(1:2, function(i) {histogram(i)})

wrap_plots(chart_list) # なぜか作図されないので、もう一度手打ちして実行する必要がある

# 設問3
data_a <- weights[1][,1]
data_b <- weights[2][,1]
result <- t.test(x = data_a, y = data_b, var.equal = TRUE,
	paired = TRUE # 対応がある
)
print(result)
cat("対立仮説 =", result$p.value < 0.05, "\n") # 設問4

# 設問5
result <- t.test(x = data_a, y = data_b, var.equal = TRUE,
	paired = FALSE # 対応がない
)
print(result)

# 設問6
# 正規性
lapply(1:2, function(i) {
	data <- weights[i][,1]
	print(ks.test(data, "pnorm", mean=mean(data), sd=sd(data)))
})

# 等分散性
print(var.test(data_a, data_b))

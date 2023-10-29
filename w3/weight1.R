
weights <- read.table("weight.txt", header=FALSE, skip=1)
list <- c("A群", "B群")
# index <- c("V1", "V")
# paired = T

histogram <- function(i) {
	w <- weights[i][,1]
	m <- mean(w)
	sd <- sd(w)
	
	# 設問(2)
	cat(list[i], "の平均値=", m, "\n", sep="")
	cat(list[i], "の標準偏差=", sd, "\n", sep="")

	# hist(w, xlab="体重", ylab="人数")
	# weights[i] %>% ggplot(aes(x = "V1")) + geom_histogram()
	ggplot(weights[i], aes(x = w)) + 
		geom_histogram() + 
		labs(x = paste(list[i], "の体重", sep=""), y = "人数")
}
chartList <- lapply(1:2, function(i) {
	histogram(i)
})

wrap_plots(chartList)

result <- t.test(
	x = weights[1][,1], 
	y = weights[2][,1],
	paired = TRUE,
	var.equal = TRUE
)
print(result)
p <- result$p.value
ha <- p < 0.05
cat("対立仮説 =", ha, "\n")
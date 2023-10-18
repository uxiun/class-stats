library("ggplot2")
library("patchwork")
library("dplyr")

weights <- read.table("weight.txt", header=FALSE, skip=1)
print(weights)

list <- c("A", "B")
# index <- c("V1", "V")
# paired = T

# w <- data.frame(matrix(nrow=nrow(weights)))

# layout(t(1:2))
# tapply(weights[1][,1], weights[2][,1], )


# for (i in 1:2) {
histogram <- function(i) {
	w <- weights[i][,1]
	# print(w)
	m <- mean(w)
	sd <- sd(w)
	cat(list[i], "の平均値=", m, "\n")
	cat(list[i], "の標準偏差=", sd, "\n")
	# hist(w, xlab="体重", ylab="人数")
	# weights[i] %>% ggplot(aes(x = "V1")) + geom_histogram()
	ggplot(weights[i], aes(x = w)) + geom_histogram()
}
chartList <- lapply(1:2, function(i) {
	histogram(i)
})

wrap_plots(chartList)
# .vsc.attach()
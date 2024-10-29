# 設問2
example <- read.table("week4-example.txt", header=TRUE)
arrowheadline <- arrow_head_line(angle=0, lineend="butt")
cols <- colnames(example)

# 設問2 発展課題
frame <- do.call(rbind.data.frame,
	lapply(cols, function(col) {
		# for文を使わず直接ここで平均と標準偏差を求めることもできる
		data.frame(weight = example[,col], group = factor(rep(col, length(example))))
	})
)

for (col in cols) {
	m <- mean(frame$weight[frame$group == col])
	cat(col, "群の平均:", round(m, 3), "\n", sep = "")
}


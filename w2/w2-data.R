d <- read.table('week2-data.txt') # 読み込む
dv <- d$V1 # V1の値列のみを抜き出す
dm <- mean(dv) # その平均を求める
ds <- sd(dv) # その標準偏差を求める
hist(dv, breaks=25, xlab="value", ylab="frequency")

# library(ggplot2, help, pos = 2, lib.loc = NULL)

d <- read.table('week2-data.txt') # 読み込む
v <- d$V1 # V1の値列のみを抜き出す
dm <- mean(v) # その平均を求める
ds <- sd(v) # その標準偏差を求める
dv <- var(v)

g <- hist(v, breaks=25, xlab="value", ylab="frequency")

# g <- ggplot(d, aes(x = V1))
# g <- g + geom_histogram(binwidth=25)
# plot(g)

# xfit <- seq(min(v), max(v), length=50)
curve(dnorm(xfit, dm, dv), min(v), max(v), add=T, col="red")
# lines(density(v), col="red") #ぺったんこ

arrows(dm-ds, 0.5, dm+ds, code=3, col="red", lwd=2)
segments(dm, 0, y1=6, col="red", lwd=2)
text(cex=1.5, dm-ds, 0.2, "平均-標準偏差")
text(cex=1.5, dm+ds, 0.2, "平均+標準偏差")
text(cex=1.5, dm+4, 5.7, "平均")


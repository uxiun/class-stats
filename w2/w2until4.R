d <- read.table('week2-data.txt') # 読み込む
# 1.に続けて実行
v <- d$V1 # V1の値列のみを抜き出す
dm <- mean(v) # その平均を求める
ds <- sd(v) # その標準偏差を求める
print(dm)
print(ds)
# 2.に続けて実行
g <- hist(v, breaks=25, xlab="value", ylab="frequency")
# 3.に続けて実行。week2-data.txtのそれぞれの範囲を以下のfileに分割した。
files <- c(
	"week2-data5.txt",
	"week2-data10.txt",
	"week2-data20.txt"
)
lengs <- c(5,10,20)
for (i in 1:3) {
	name <- files[i]
	f <- read.table(name)
	d <- f$V1
	m <- mean(d)
	sa <- ds / sqrt(lengs[i]) # 標準誤差を求める
	print(name)
	cat("平均=")
	print(m)
	cat("標準誤差=")
	print(sa)
}

curve(dnorm(x, dm, ds), min(v), max(v), add=TRUE, col="red", lwd=2) # 縦軸を度数にしたまま潰れないようにするやり方がわかりませんでした
# lines(density(v), col="red") 
arrows(dm-ds, 0.5, dm+ds, code=3, col="red", lwd=2)
segments(dm, 0, y1=6, col="red", lwd=2)
text(cex=1.5, dm-ds, 0.2, "平均-標準偏差")
text(cex=1.5, dm+ds, 0.2, "平均+標準偏差")
text(cex=1.5, dm+4, 5.7, "平均")
gosa <- ds / sqrt(50)
text(cex=1.5, dm+5, 5, "標準誤差")
text(cex=1.5, dm+15, 5, gosa)

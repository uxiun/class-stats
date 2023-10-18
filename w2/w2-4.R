files <- c(
	"week2-data5.txt",
	"week2-data10.txt",
	"week2-data20.txt"
)
lengs <- c(5,10,20)
sd <- 12.97987
for (i in 1:3) {
	name <- files[i]
	f <- read.table(name)
	d <- f$V1
	m <- mean(d)
	sa <- sd / sqrt(sd(d))
	print(name)
	cat("平均=")
	print(m)
	cat("標準誤差=")
	print(sa)
}
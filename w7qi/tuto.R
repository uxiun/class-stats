
set.seed(123)
# 標準正規分布N(0,1)乱数
data <- c(rnorm(100, mean= 0, sd= 1))
n <- 100
r <- 100

bs <- sapply(1:r, function(x) {
	return(mean(sample(data, n, replace=T)))
})

bs100 <- numeric(r) # 要素0で初期化
for (i in 1:r) bs100[i] <- mean(sample(data,n, replace=T))

# bs, bs100 は同等

hist(bs, freq= T)
bs.info <- c(
	mean= mean(bs),
	var= var(bs),
	sd= sd(bs)
)

hist(bs, freq=F, add=F)
lines(density(bs))
quantile(bs, p= c(0.025, 0.975))



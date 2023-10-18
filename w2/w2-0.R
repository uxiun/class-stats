sm <- numeric(length=10000)
for (i in 1:10000) {
	s <- rnorm(n=10,mean=100,sd=10) #N(100,10^2)の正規分布から無作為に標本抽出
	sm[i] <- mean(s) #標本平均を求める
}
meanSm <- mean(sm) #標本平均の平均を求める
print(meanSm)
hist(sm)

# after running weight.R
# 正規性
lapply(c(dataA, dataB), function(data) {
	print(ks.test(data, "pnorm", mean=mean(data), sd=sd(data)))
})

# 等分散性
print(var.test(dataA, dataB))


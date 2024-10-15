

# 非復元抽出
	A <- rpois(100,50)
	B <- rpois(100,52)

	hist(A, breaks= 20, col= "#ff00ff40")
	hist(B, breaks= 20, col= "#0000ff40", add= TRUE)
	Diff <- abs(mean(A) - mean(B))
	Data <- c(A,B)
	data_r <- sample(Data, replace= FALSE) # 随机排序 replace=F: 非復元
	
	#新群
	A_r <- data_r[1:length(A)] 
	B_r <- data_r[(length(A)+1): length(data_r)]
	
	hist(A_r, breaks= 10, col= "#ff00ff40")
	hist(B_r, breaks= 10, col= "#ff00ff40", add= TRUE)
	
	Diff_rs <- abs(mean(A_r) - mean(B_r))
	
	cat("Diff", Diff, "\n")
	cat("Diff_rs", Diff_rs, "\n")

# 随机化検定
	n <- 4999
	Diff_r <- numeric(n+1)
	Data <- c(A,B)
	
	for (i in 1:n) {
		data_r <- sample(Data, replace=F)
		A_r <- data_r[1: length(A)]
		B_r <- data_r[(length(A)+ 1): length(data_r)]
		Diff_r[i] <- abs(mean(A_r) - mean(B_r))
	}
	
	Diff_r[n+1] <- Diff 
	largerCount <- sum(Diff_r >= Diff) # Diff_r.filter((r,i) => r >= Diff[i]).length に相当。i番目の要素がDiffのそれより大きいものを数える
	p <- largerCount/ (n+1)
	cat("largerCount", largerCount, "\n")
	cat("p", p, "\n")

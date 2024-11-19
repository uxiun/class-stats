# 設問(1)
	example <- read.table("data_r.txt", header=TRUE)
	hist(example$A, breaks= 20, col= "#ff00ff40")
	hist(example$B, breaks= 20, col= "#0000ff40", add=FALSE)

# 設問(2)
	random_p <- function(n, example, showHist) {
		Diff <- abs(mean(example[,1]) - mean(example[,2]))

		Diff_r <- numeric(n+1)
		Data <- unlist(example, use.names=F)

		for (i in 1:n) {
			data_r <- sample(Data, replace=F)
			A_r <- data_r[1: length(example[,1])]
			B_r <- data_r[(length(example[,1])+1): length(data_r)]
			Diff_r[i] <- abs(mean(A_r) - mean(B_r))
		}

		Diff_r[n+1] <- Diff
		p <- sum(Diff_r >= Diff)/ (n+1)

		if (showHist) {
			hist(Diff_r, breaks=20)
			segments(Diff, 2000, y1=0, col="red", lwd=2)
			text(cex=1.5, Diff+0.5, 1200, paste("p値", p))
		}
		return(p)
	}

	cat("p値", random_p(9999, example, TRUE), "\n")

# 設問(3)
	
	pvalues <- sapply(1:10, function(i) { return(random_p(50, example, FALSE)) })
	print("pvalues=")
	print(pvalues)

	p_mean <- mean(pvalues)
	p_sd <- sd(pvalues)
	cat("p値平均", p_mean, "\n")
	cat("p値標準偏差", p_sd, "\n")

	# お手本通り
	data <- unlist(example, use.names=FALSE)
	Diff <- abs(mean(example[,1]) - mean(example[,2]))

	ps <- 0
	mean_p <- 0
	sd_p <- 0

	r <- 10
	n <- 25
	ns <- 0
	m <- 0

	while (n < 50000) {
		n <- n*2
		m <- m + 1
		for (j in 1:r) {
			Diff_r <- 0
			for (i in 1:n) {
				data_r <- sample(data, replace = FALSE)
				A_r <- data_r[1:length(example[,1])]
				B_r <- data_r[(length(example[,1])+1):length(data_r)]
				Diff_r[i] <- abs(mean(A_r) - mean(B_r))
			}
			Diff_r[n+1] <- Diff
			p <- sum(Diff_r >= Diff)
			ps[j] <- sum(Diff_r >= Diff)/(n+1)
			mean_p[m] <- mean(ps)
			sd_p[m] <- sd(ps)
			ns[m] <- n
		}
	}

	plot(x = ns, y = sd_p, log = "x", xlab = "Iteration", ylab = "SD(P)")
	plot(x = ns, y = mean_p, log = "x", xlab = "Iteration", ylab = "Mean(P)")

# 設問(4)
	res_t <- t.test(
		x= example$A,
		y= example$B,
		alternative= "greater",
		paired= FALSE,
		var.equal= TRUE,
		conf.level= 0.95,
	)
	print(res_t)

# 設問(5)
	library("perm")
	res_perm <- permTS(
		x= example$A,
		y= example$B,
		alternative= "greater",
	)
	print(res_perm)

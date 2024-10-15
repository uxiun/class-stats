
# 設問(1)
	example <- read.table("data_r.txt", header=TRUE)
	cols <- colnames(example)
	hist(example$A, breaks= 20, col= "#ff00ff40")
	hist(example$B, breaks= 20, col= "#0000ff40", add= TRUE)


# 設問(2)
	random_p <- function(n, example, showHist) {
		Diff <- abs(mean(example[,1]) - mean(example[,2]))
		
		Diff_r <- numeric(n+1)
		Data <- unlist(example, use.names=F)
		
		for (i in 1:n) {
			data_r <- sample(Data, replace=F)
			A_r <- data_r[1: length(example[,1])]
			B_r <- data_r[(length(example[,2])+1): length(data_r)]
			Diff_r[i] <- abs(mean(A_r) - mean(B_r))
		}
		
		Diff_r[n+1] <- Diff
		p <- sum(Diff_r >= Diff)/ (n+1)
		
		if (showHist) {
			hist(Diff_r)
			# arrows(p, -1, p, 0, code=3, col="red", lwd=2)
			segments(Diff, 2000, y1=0, col="red", lwd=2)
			text(cex=1.5, Diff+0.3, 500, cat("p値", p))
		}
		return(p)
	}
		
	cat("p値", random_p(9999, example, T), "\n")

# 設問(3)
	
	# 範囲と長さの指定から適当な階差数列を作る函数
	seq_step_resultlen <- function(start, end, resultlen) {
		return(seq(start, end, (end - start)%/%(resultlen - 1)))
	}
	
	pvalues <- sapply(1:10, function(i){
		return(random_p(50, example, F))
	} )
	print("pvalues=")
	print(pvalues)
	
	pmean <- mean(pvalues)
	psd <- sd(pvalues)
	cat("p値平均", pmean, "\n")
	cat("p値標準偏差", psd, "\n")
	
	ptuples <- lapply( seq_step_resultlen(50, 50000, 11) , function(i) {
		pvalues <- sapply(1:10, function(j) { 
			return(random_p(i, example, FALSE))
		})
		pmean <- mean(pvalues)
		psd <- sd(pvalues)
		return(c(i, pmean, psd))
	})
	
	print(ptuples)
	
	transposed <- transpose(ptuples)
	print(transposed)
	
	values <- map(transposed,
		function(x) { return(unlist(x, use.names= F)) }
	)
	flat <- map(ptuples,
		function(x) { return(setNames(unlist(x, use.names= F), c("n", "p_mean", "p_sd"))) }
	)
	
	datas <- dplyr::bind_rows(flat) #flattenした各行をdata.frameに纏める
	
	ggplot(datas, aes(n, p_mean))+
		geom_point()+
		geom_line()
	
	ggplot(datas, aes(n, p_sd))+
		geom_point()+
		geom_line(linetype= "dashed")


# 設問(4)
	res.perm <- permTS(
		x= example$A, 
		y= example$B, 
		alternative= "greater",
	
	)
	print(res.perm)
	
	res.t <- t.test(
		x= example$A,
		y= example$B,
		alternative= "greater",
		paired= FALSE,
		var.equal= TRUE,
		conf.level= 0.95,
	)
	print(res.t)
	

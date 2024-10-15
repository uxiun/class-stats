
# 設問(1)
	example <- read.table("data_r.txt", header=TRUE)
	cols <- colnames(example)
	hist(example$A, breaks= 20, col= "#ff00ff40")
	hist(example$B, breaks= 20, col= "#0000ff40", add= TRUE)

	# arrowheadline <- arrow_head_line(angle=0, lineend="butt")
# histogram <- function(i) {
# 	c <- cols[i]
# 	w <- example[,c]
# 	m <- mean(w)
# 	sd <- sd(w)
	
# 	cat(c, "の平均値=", m, "\n", sep="")
# 	cat(c, "の標準偏差=", sd, "\n", sep="")
	
# 	ggplot(example[i], aes(x= w)) + 
# 		geom_histogram()+
# 		labs(x= paste(c, "の体重", sep=""), y= "個体数") +
# 				geom_segment(aes(x= m,    y= -0.3, xend= m, yend= 5), colour="red")+
# 		geom_arrow_segment(aes(x= m-sd, y= -0.3, yend= 0), color= 2)+
# 		geom_arrow_segment(aes(x= m+sd, y= -0.3, yend= 0), color= 2)+
# 		geom_arrow_segment(aes(xend= m - sd, y= 5, x= m), color= 2)+
# 		geom_arrow_segment(aes(xend= m + sd, y= 5, x= m), color= 2)+
# 		annotate("text", x= m, y= 5.2, label="平均")+
# 		annotate("text", x= m-sd, y= 5.5, label="平均-標準偏差")+
# 		annotate("text", x= m+sd, y= 5.5, label="平均+標準偏差")+
# 		annotate("text", x= m, y= -0.5, label= round(m, 2))+
# 		annotate("text", x= m-sd, y= -0.5, label= round(m-sd, 2))+
# 		annotate("text", x= m+sd, y= -0.5, label= round(m+sd, 2))
# }
# ggs <- lapply(1:length(example), function(i) {
# 	histogram(i)
# })
# wrap_plots(ggs)

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
		# return(c(i, random_p(i, example, FALSE)) )
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
	print(flat)
	print(values)
	
	# datas <- data.frame(values
	# 	#, row.names= c("n", "p.mean", "p.sd")
	# )
	
	# datas <- bind_rows(lapply(ptuples, as.data.frame.list))
	# datas <- do.call(rbind.data.frame, flat)
	datas <- dplyr::bind_rows(flat)
	print(datas)
	print(datas$n)
	
	# plotv <- function(data, aesxy, keta) {
	# 	# dat <- dplyr::bind_cols( c(data[aesxy[1]], data[aesxy[2]]) )

	# 	print(data[[aesxy[1]]])
		
	# 	x <- data[[aesxy[1]]]
	# 	y <- data[[aesxy[2]]]
	# 	# dat <- data.frame(
	# 	# )
	# 	dat <- data
	# 	print(dat)
	# 	dat$text <- with(dat, paste(round(,keta[1]), round(y,keta[2]), sep=", ") )
	# 	gp <- ggplot(
	# 		data= dat,
	# 		aes(x= aesxy[1], y= aesxy[2], label= text)
	# 	)+
	# 	geom_point()+
	# 	geom_text(
	# 		size= 4,
	# 		hjust= 1,
	# 		vjust= 1
	# 	)
	# 	return(gp)
	# }
	
	# plotv(datas, c("n", "p_mean"), c(2, 3))+
	# plotv(datas, c("n", "p_sd"), c(2, 3))
	
	# plot(x= datas$n, y= datas$p_mean, type="b")
	# plot(x= datas$n, y= datas$p_sd, add= T, type="b" ,lty= 2)
	
	# datas %>%
	# 	gather(ynames, cols, -n) %>%
	# 	ggplot(aes(x= n, y= cols))+
	# 	geom_line(aes(linetype= ynames))
	
	# print(gg)
	
	
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
	

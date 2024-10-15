library("tidyverse")
library("patchwork")

# 1

datap <- read.table('data_p.txt', header = TRUE, stringsAsFactors = FALSE)

# (1)
	
	data.trans <- map(transpose(datap), \(row) lapply(1:length(row), function(i) {
		return(
			list(ab= names(row)[i], x= row[[i]] )
		)
	})) # list(list(ab= A|B, x= double)[2] ) に変換
	
	gg.origin.src <- list_flatten(data.trans) # list(ab= A|B, x= double) に変換
	gg.origin.data <- bind_rows(gg.origin.src) # data.frame に格納
	gg.origin <- ggplot(gg.origin.data, aes(x= x, group= as.factor(ab)))+
		geom_histogram(
			alpha= 0.4,
			position= "identity",
			aes(fill= ab))
	print(gg.origin)

# (2)
	
	# A,Bそれぞれについて、無作為に抽出した値の平均値を1000個作る。
	bss <- lapply(colnames(datap), function(col) {
		data <- datap[col][,1]
		bs <- lapply(1:1000, function(x) {
			return(mean(sample(data, length(data)%/% 10, replace= T)) )
		})
		return(bs)
	})
	
	# 差の絶対値を取る。
	diffs <- list_c(map(transpose(bss), \(ab) abs(ab[[1]] - ab[[2]]) ))
	
	df.diffs <- bind_cols(list(diff= diffs))
	diff.origin <- abs(mean(datap$A) - mean(datap$B))
	diff.origin.y <- 0.15
	ggplot(df.diffs, aes(x= diff))+
		geom_histogram(aes(y= ..density..))+
		geom_density()+
		geom_segment(aes(x= diff.origin, y= 0, xend= diff.origin, yend= diff.origin.y))+
		annotate("text", x= diff.origin, y= diff.origin.y, label= paste("実測値", diff.origin))
		
	quantile.p <- c(0.025, 0.975)
	diff.less <- 0
	diff.over <- 0
	for (d in diffs) {
		if (d < diff.origin) {
			diff.less <- diff.less+1
		} else {
			diff.over <- diff.over+1
		}
	}
	pvalue <- diff.less / length(diffs)
	print(pvalue)
	
	# col.ab <- list_c(
	# 	map(1:length(bss), \(i) {
	# 		rep(colnames(datap)[i], length(bss[[i]]))
	# 	})
	# )
	# col.mean <- list_c(list_c(bss))
	
	# datagg <- dplyr::bind_cols(list(
	# 	ab= as.factor(col.ab),
	# 	mean= col.mean
	# ))
	
	# quantiles <- lapply(bss, function(bs) {
	# 	return(quantile(list_c(bs), p= quantile.p))
	# })
		
	# df.quant.ab <- map(colnames(datap), \(col) rep(col, length(quantile.p)))
	# df.quant <- dplyr::bind_cols(list(
	# 	ab= list_c(df.quant.ab),
	# 	quantile= list_c(quantiles)
	# ))
	
	# mean.origin <- bind_rows(
	# 	map(1:length(datap), \(i) list(
	# 		ab= as.factor(colnames(datap)[i]),
	# 		mean= mean(datap[[i]])
	# 	))
	# )
	
	# df.quantile <- dplyr::bind_rows(
	# 	map(1:length(quantiles), \(i) c(
	# 		ab= as.factor(colnames(datap)[i]),
	# 		quantiles[[i]][1],
	# 		quantiles[[i]][2]
	# 	))
	# )
	
	# yend <- -0.02
	# y.origin <- 0.45
	# gg <- ggplot(datagg, aes(x= mean, group= ab))+
	# 	geom_histogram(alpha=0.4, position="identity", aes(y= ..density.., fill= ab))+
	# 	geom_density(aes(colour= ab))+
		
	# 	geom_segment(
	# 		data= mean.origin,
	# 		aes(x= mean, y= 0, xend= mean, yend= y.origin, colour= ab)
	# 	)+
	# 	geom_text(
	# 		data= mean.origin,
	# 		aes(y= y.origin + 0.02, label= paste("実測値\n", as.character(round(mean, 1)), sep=""), colour= ab))
	# 	# annotate("text", x= mean,)
	# print(gg)
	
	# gg+
	# 	geom_curve(
	# 		data= df.quantile,
	# 		aes(x= `2.5%`, xend= `97.5%`, y= 0, yend= 0, colour= ab)
	# 	)
	# 	annotate(aes(y= 0, label=""))
	# print(gg)

# (4)
res.t <- t.test(
	x= datap$A,
	y= datap$B,
)
print(res.t)

# 2

is.inrange <- function(x,y,r) {
	return(x^2 + y^2 < r^2)
}


set.seed(82)

# (1)
	pis <- lapply(1:500, function(i) {
		k <- i*10
		r <- 1
		iss <- sapply(1:k, function(i) {
			x <- runif(r)
			y <- runif(r)
			return(is.inrange(x,y,r))
		})
		
		truescount <- 0
		for (is in iss) {
			if(is) {
				truescount <- truescount+1
			}
		}
		
		return(c(k= k, pi= 4*truescount/length(iss)))
	})
	
	k.pi.df <- dplyr::bind_rows(pis)
	
	gg1 <- ggplot(k.pi.df, aes(x= k, y= pi))+
		geom_line()
	print(gg1)

# (2)	
	pi.mean.vars <- lapply(1:500, function(i) {
		k <- i*10
		r <- 1
		
		pis <- sapply(1:100, function(j) {
			
			truescount <- 0
			for (i in 1:k) {
				x <- runif(r)
				y <- runif(r)
				if (is.inrange(x,y,r)) {
					truescount <- truescount+1
				}
			}
			
			return(4*truescount/k)
		})
		
		return(c(k= k, mean= mean(pis), var= var(pis)))
	})
	
	pi.mean.var.df <- dplyr::bind_rows(pi.mean.vars)
	
	gg2.x <- ggplot(pi.mean.var.df, aes(x= k)) # kをx軸として
	gg2.mean <- gg2.x+ geom_line(aes(y= mean)) # y軸が平均値
	gg2.var <- gg2.x+ geom_line(aes(y= var)) # y軸が分散
	wrap_plots(list(gg2.mean, gg2.var))

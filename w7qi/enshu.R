library("tidyverse")
library("patchwork")

# 演習課題
set.seed(482)
randoms <- c(rnorm(2000, mean= 0, sd= 5))

# 1
	bs <- sapply(1:100, function(x) {
		return(mean(sample(randoms, 10, replace=T)))
	})
	hist(bs)
	bs.info <- c(
		mean= mean(bs),
		sd= sd(bs)
	)

# 2
	# start*hi^i <= maxe を満たす最大のiを求める
	calc.tosa.times <- function(hi, start, maxe) {
		i <- 0
		while (start*hi^i <= maxe) {
			i <- i+1
		}
		return(i-1)
	}
	
	r <- calc.tosa.times(2, 10, 10240)
	pickrs <- c(10,40,320)
	picks <- list() #r=10, 40, 320の場合のbsを保存する
	bssrs <- c() # 反復回数の列
	bss <- lapply(0:r, function(i) {
		r <- 10*2^i
		l <- sapply(pickrs, function(x) { x == r}) # pickrsの各要素がrと等しいかを表す二値のvector
		
		bs <- sapply(1:r, function(x) {
			return(mean(sample(randoms, 100, replace=T)))
		})
		
		# pickrsにrと等しいものがあればpicksにbsを入れる
		# lapply外の親環境に代入するために<<-を使う
		if (any(l)) {
			print(picks)
			picks <<- append( picks, list(bs)) 
		}
		bssrs <<- append(bssrs, r)
		return(bs)
	})
	bss.means <- sapply(bss, function(bs) {
		return(mean(bs))
	})
	# 各反復回数の平均値
	bss.means <- setNames(bss.means, bssrs)
	print(bss.means)
	
	# lapply(1:length(pickrs), function(i) {
	# 	bs <- picks[[i]]
	# 	r <- pickrs[i]
	# 	hist(bs, add= T)
	# })
	
	# pickrs: vector(3), picks: list<vector>(3) から rowss: list<list<vector(2)>>(3) を作る
	rowss <- lapply(1:length(pickrs), function(i) {
		bs <- picks[[i]]
		r <- pickrs[i]
		v <- lapply(bs, function(b) {
			return(c(r= r, b= b)) # あとでdata.frameにする際の各行に相当
		})
		return(v)
	})
	
	rows <- list_flatten(rowss) # list<list<vector(2)>>(3)をlist<vector(2)>(10+40+320) にする
	df <- dplyr::bind_rows(rows) #data.frameに変換
	df <- transform(df, r= as.factor(as.integer(r))) #r列を整数型そして要因型に変換
	
	ggplot(df, aes(x= b, group= r, fill= r))+
		geom_histogram(alpha=0.4, position="identity")+
		geom_line(stat="density", aes(colour= r))
		# geom_density(aes(colour= r))+

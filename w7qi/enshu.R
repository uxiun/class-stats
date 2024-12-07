# library("tidyverse")
# library("patchwork")

# 演習課題
set.seed(482) # 乱数の種を設定
randoms <- c(rnorm(2000, mean= 0, sd= 5)) # 平均0 分散5 の正規分布から2000個の乱数を生成して、vectorに収める

# # 1
# 	bs <- purrr::map_vec(1:100, \(x)
# 		mean(sample(randoms, 10, replace=TRUE))
# 	)

# 	bs.mean <- mean(bs)
# 	bs.var <- var(bs)
# 	bs.sd <- sd(bs)

# 	bs_frame <- data.frame(x= bs)

# 	# hist(bs, freq=FALSE)
# 	# lines(density(bs))
# 	# quantile(bs, p= c(0.025, 0.975))
# 	arrowheadline <- arrow_head_line(angle=0, lineend= "butt")
# 	gg1 <- ggplot(bs_frame, aes(x= x))+
# 		geom_histogram()+
# 		annotate("segment", x = bs.mean, y = 0, xend = bs.mean, yend = 9, color = "red")+
# 		annotate("segment", x = bs.mean - bs.sd, y = 0, xend = bs.mean - bs.sd, yend = 7, color = "red")+
# 		annotate("segment", x = bs.mean + bs.sd, y = 0, xend = bs.mean + bs.sd, yend = 7, color = "red")+
# 		# geom_arrow_segment(aes(x = bs.mean - bs.sd, y = -0.3, yend = 0), color= 2)+
# 		# geom_arrow_segment(aes(x = bs.mean + bs.sd, y = -0.3, yend = 0), color= 2)+
# 		annotate("text", x = bs.mean, y = 8, label=paste("平均", round(bs.mean, 2)), color = "red")+
# 		annotate("text", x = bs.mean + bs.sd, y = 8, label= paste("標準偏差", round(bs.sd, 2)), color = "red")+
# 		annotate("text", x = bs.mean, y = -0.3, label= round(bs.mean, 2))+
# 		annotate("text", x = bs.mean - bs.sd, y = -0.3, label= round(bs.mean - bs.sd, 2))+
# 		annotate("text", x = bs.mean + bs.sd, y = -0.3, label= round(bs.mean + bs.sd, 2))

# 	print(gg1)


rs <- c() #繰り返し回数のvector
r <- 10
# rs: [1]    10    20    40    80   160   320   640  1280  2560  5120 10240 となるよう数値を入れていく
while (r <= 10240) {
	rs[length(rs)+1] <- r
	r <- r*2
}

pickrs <- c(20,320)
picks <- list() #r=20, 320の場合のbsを保存する
bss <- purrr::map(rs, \(r) {
	bs <- sapply(1:r, \(x) mean(sample(randoms, 100, replace=TRUE))) # bootstrap

	# pickrsにrと等しいものがあればpicksにbsを入れる
	if (any(sapply(pickrs, \(x) { x == r }))) {
		picks <<- append(picks, list(bs)) # scope外の環境に代入するために<<-を使う
	}

	list(bs)
})

bsdf <- data.frame(
	r = as.factor(rs),
	mean = purrr::map_vec(bss, \(bs) mean(bs[[1]]))
)
print("bsdf")
print(bsdf)

# 各反復回数の平均値
print(ggplot(bsdf, aes(x = r))+
	geom_point(size = 4, aes(y = mean))+
	labs(title = "繰り返し回数ごとの平均値", x = "繰り返し回数", y = "平均")
)

# pickrs: vector(長さ2), picks: list<vector(各r)>(2) から rowss: list<list<vector(r=,b=)>(各r)>(2) を作る
rowss <- lapply(seq_along(pickrs), \(i) {
	bs <- picks[[i]]
	r <- pickrs[i]
	v <- lapply(bs, \(b) c(r= r, b= b)) # あとでdata.frameにする際の各行に相当
	return(v)
})

rows <- list_flatten(rowss) # list<list<vector(r=,b=)>>(2)を1階層つぶしてlist<vector(r=,b=)>(20+320) にする
df <- dplyr::bind_rows(rows) #data.frame(r=,b=)に変換
df <- transform(df, r= as.factor(r)) #r列を要因型に変換

print(ggplot(df, aes(x= b, group= r, fill= r))+ # rでgroup分けし、塗りつぶす色(fill)を紐づける
	geom_density(aes(colour= r), # rで色分け
		fill = NA, # 塗らつぶさず線だけ表示
		alpha = 0.3)+ # 透明度設定
	geom_histogram(aes(y= after_stat(density)), # y軸を密度にして正規化
		position="identity", # 重ねる
		alpha= 0.5)
)

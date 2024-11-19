library("tidyverse")

set.seed(482) # 乱数の種を設定
randoms <- c(rnorm(2000, mean= 0, sd= 5)) # 平均0 分散5 の正規分布から2000個の乱数を生成して、vectorに収める

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

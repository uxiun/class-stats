# pickrs: vector(3), picks: list<vector>(3) から rowss: list<list<vector(r=,b=)>(各r)>(3) を作る
	rowss <- lapply(seq_along(rs), function(i) {
		bs <- bss[[i]]
		r <- rs[i]
		v <- lapply(bs, function(b) {
			return(c(r= r, b= b)) # あとでdata.frameにする際の各行に相当
		})
		return(v)
	})

	rows <- list_flatten(rowss) # list<list<vector(2)>>(3)をlist<vector(2)>(20+320) にする
	df <- dplyr::bind_rows(rows) #data.frameに変換
	df <- transform(df, r= as.factor(as.integer(r))) #r列を整数型そして要因型に変換

	print(ggplot(df, aes(x= b, group= r, fill= r))+ # rでgroup分けし、色(fill)を紐づける
		geom_density(aes(colour= r), # rで色分け
			fill = NA, # 塗らずに線だけ表示
			alpha = 0.3) # 透明度設定
		# geom_histogram(aes(y= after_stat(density)), # y軸を密度にして正規化
			# position="identity", # 重ねる
			# alpha= 0.5)
	)

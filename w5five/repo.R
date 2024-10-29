example <- read.table("score_ABC.txt", header=TRUE)

# 設問1
classNames <- c("A","B","C")
subjectNames <- c("jp", "math", "eng")

	# 平均値と標準偏差
	## 方法1
	meanexample <- c()
	sdexample <- c()
	ncolexample <- ncol(example)
	for (i in 1: ncolexample){
		meanexample <- c(meanexample, mean(example[, i]))
		sdexample <- c(sdexample, sd(example[, i]))
	}

	print(meanexample)
	print(sdexample)

	classColumn <- list_c(lapply(classNames, function(class) {rep(class, length(subjectNames))}))
	meanFrame <- data.frame(
		class = classColumn,
		subject = rep(subjectNames, length(classNames)),
		mean = meanexample,
		sd = sdexample
	)

	print(meanFrame)

	## 方法2
	meanFrameRows <- purrr::imap(classNames, \(class, i)
		imap(subjectNames, \(subject, j) {
			d <- example[, (i-1)*length(classNames) + j]
			list(
				class= class,
				subject= subject,
				mean= mean(d),
				sd= sd(d) # trailing comma not allowed!
			)
		})
	) |> list_flatten()

	meanFrame1 <- dplyr::bind_cols(list_transpose(meanFrameRows))
	print(meanFrame1)

	# boxplot
	boxplot(example)

# 設問2
nrowexample <- nrow(example)
classes <- gl(length(classNames), nrowexample * length(subjectNames), label=classNames)
# classes <- factor(unlist(lapply(classNames, function(s) rep(s, nrowexample * length(subjectNames)) )))
subjects <- rep( gl(length(subjectNames), nrowexample, label=subjectNames), times= length(classNames) )
scores <- as.numeric(as.matrix(example))

## 確認用
aovframe <- data.frame(class=classes, subject=subjects, score=scores)
print(aovframe)

print(summary( aov(scores~ classes*subjects) ))

# 設問3, 4省略

# 設問5
interaction.plot(classes, subjects, scores)
interaction.plot(subjects, classes, scores)

# 設問6
tukey <- TukeyHSD(aov(scores~subjects))
print(tukey)

# 設問7 1対応分散分析

count <- imap(classNames, \(class, i) example[, (i-1)*length(subjectNames)+1] |> length()) |> list_c()
cat("count\n")
print(count)

id <- 0
students <- list()
for (i in seq_along(classNames)) {
	len <- count[i]
	students[[i]] <- rep(factor((id+1):(id+len)), length(subjectNames))
	id <- id + len
}

print("students")
print(students)

students <- list_c(students)
aovres <- aov(scores~ classes*subjects + Error(students:classes + students:classes:subjects))
print(summary(aovres))
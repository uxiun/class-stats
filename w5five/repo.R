

example <- read.table("score_ABC.txt", header=TRUE)

# 設問（1）
meanexample <- c(0);
sdexample <- c(0);
ncolexample <- ncol(example)
for (i in 1: ncolexample){
	meanexample <- c(meanexample, mean(example[, i]));
	sdexample <- c(sdexample, mean(example[, i]));
}
print(meanexample)
print(sdexample)
boxplot(example)

# 設問（2）
nrowexample <- nrow(example)
subjectNames <- c("jp", "math", "eng")
subjectJME <- rep( gl(3, nrowexample, label=subjectNames), times= ncolexample/ length(subjectNames) )
classNames <- c("A","B","C")
classABC <- unlist(lapply(classNames, function(s) rep(s, nrowexample * length(subjectNames)) ))
scores <- as.numeric(as.matrix(example))
aovframe <- data.frame(class=classABC, subject=subjectJME, score=scores)

print(scores)
print(aovframe)


factorClass <- factor(aovframe$class)
resaov <- aov(aovframe$score~ factorClass)
print(summary(resaov))
factorSubject <- factor(aovframe$subject)
resaov <- aov(aovframe$score~ factorSubject)
print(summary(resaov))

resaov <- aov(aovframe$score~ factorClass*factorSubject)
print(summary(resaov))

# 設問(5)
interaction.plot(factorClass, factorSubject, aovframe$score)
interaction.plot(factorSubject, factorClass, aovframe$score)



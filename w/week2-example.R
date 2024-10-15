table <- read.table("week2-data.txt", header=FALSE)
data <- table[,1]
mean <- mean(data)
sd <- sd(data)
print(mean)
print(sd)
hist(data, breaks=5, xlab="value", ylab="frequency", main="histogram")
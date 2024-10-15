Data <- read.table("week3-example.txt", header=TRUE)
result <- t.test(Data$previous, Data$current, var.equal = TRUE, paired = TRUE)
print(result)

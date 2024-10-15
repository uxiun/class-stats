# weighs <- read.table("week4-example.txt", header=FALSE, skip=1)
example <- read.table("week4-example.txt", header=TRUE)
list <- lapply(colnames(example), function(c) {paste(c, "群", sep="")})

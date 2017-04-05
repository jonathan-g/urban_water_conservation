
# find longest run above or below threshold
x <- sample(10,100, replace=T)
thold <- 3

## use run lenth encoder 
a <- rle(x <= thold)

## use tapply to find longest run
b <- min(tapply(a$lengths, a$values, max))


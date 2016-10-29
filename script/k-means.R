kdata <- data_2016[,c(17, 29:38, 47, 49, 51)]
kdata <- sapply(kdata, as.integer)
map1 <- function(x) {
     x[x == 3] <- 6
     x[x == 2] <- 3
     return(x)
}
kdata[, 1:11] <- map1(kdata[, 1:11])
kdata[, 12:13] <- kdata[, 12:13] - 1




kdata[is.na(kdata)] <- 0

kdata[kdata[,1] == 3, 1] <- 6
kdata[kdata[,1] == 2, 1] <- 3
kdata[kdata[,] == 3, 1] <- 6
kdata[kdata[,1] == 2, 1] <- 3
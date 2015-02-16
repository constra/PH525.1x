### Probability
dat <- read.csv("femaleMiceWeights.csv")
mean(dat[13:24,2]) - mean(dat[1:12,2])

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
abline(h=sapply(s, mean), col=1:2)

highfat = s[["hf"]]
highfat

sample(highfat,6)

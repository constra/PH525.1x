library(ggplot2)

### Probability
dat <- read.csv("femaleMiceWeights.csv")
mean(dat[13:24,2]) - mean(dat[1:12,2])

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
abline(h=sapply(s, mean), col=1:2)

highfat = s[["hf"]]
highfat

sample(highfat,6)

highfat >30
as.numeric(highfat >30)
sum(highfat >30)

sum(highfat >30)/length(highfat)

population <- read.csv("femaleControlsPopulation.csv")
≈

control <- sample(population[,1,12])
mean(control)

n <- 100000
null <- vector("numeric",n)
for(i in 1:n){
        control <- sample(population[,1],12)
        treatment <- sample(population[,1],12)
        null[i] <- mean(treatment) - mean(control)
}

diff <- mean(dat[13:24,2]) - mean(dat[1:12,2])
mean(null>diff)


mean(population[,1])
population <- population[,1]
sampleMean <- replicate(10000, mean(sample(population, 12)))
plot(sampleMean)
hist(sampleMean)
null <- replicate(10000,mean(sample(population,12))-mean(sample(population,12)))
plot(null)
hist(null)
abline(v=diff,col="red")

# using ggplot2
q<-qplot(null,binwith=.3)
q+geom_vline(xintercept=diff,color="red")
